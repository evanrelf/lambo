{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Lambo.Parser
  ( parse
  , parseTokens
  )
where

import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Data.Fix (Fix (..))
import Data.Foldable (asum)
import Data.Text (Text)
import Lambo.Lexer (Token (..), lex)
import Lambo.Syntax (Expression, ExpressionF (..))
import Text.Earley ((<?>))
import Prelude hiding (lex)

import qualified Data.Text as Text
import qualified Text.Earley as Earley


parse :: Text -> Either Text Expression
parse = lex >=> parseTokens


parseTokens :: [Token] -> Either Text Expression
parseTokens tokens =
  case Earley.fullParses (Earley.parser grammar) tokens of
    ([], report) ->
      Left $ Text.pack (show report)

    (result : _, _) ->
      Right result


grammar :: Earley.Grammar r (Earley.Prod r Text Token Expression)
grammar = mdo
  let rule name prod = Earley.rule (prod <?> name)

  let inParens prod = do
        _ <- Earley.token Token_OpenParen
        x <- prod
        _ <- Earley.token Token_CloseParen
        pure x

  identifierProd <- rule "identifier" do
    Earley.terminal \case
      Token_Identifier name -> Just name
      _ -> Nothing

  indexProd <- rule "index" do
    _ <- Earley.token Token_At
    index <- Earley.terminal \case
      Token_Number number -> Just number
      _ -> Nothing
    pure index

  variableProd <- rule "variable" do
    name <- identifierProd
    index <- indexProd <|> pure 0
    pure (ExpressionF_Variable name index)

  abstractionProd <- rule "abstraction" do
    _ <- Earley.token Token_Lambda
    argument <- identifierProd
    _ <- Earley.token Token_Dot
    definition <- expressionProd
    pure (ExpressionF_Abstraction argument definition)

  applicationProd <- rule "application" do
    function <- expressionProd
    argument <- expressionProd
    pure (ExpressionF_Application function argument)

  expressionProd <- rule "expression" do
    asum
      [ Fix <$> variableProd
      , Fix <$> abstractionProd
      , Fix <$> applicationProd
      , inParens expressionProd
      ]

  pure expressionProd
