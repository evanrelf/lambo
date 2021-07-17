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

import Text.Earley ((<?>))

import Control.Monad ((>=>))
import Data.Fix (Fix (..))
import Data.Foldable (asum)
import Data.Text (Text)
import Lambo.Lexer (Token (..), lex)
import Lambo.Syntax (Expression, ExpressionF (..))
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

  variableProd <- rule "variable" do
    Earley.terminal \case
      Token_Variable name -> Just name
      _ -> Nothing

  abstractionProd <- rule "abstraction" do
    _ <- Earley.token Token_Lambda
    variable <- variableProd
    _ <- Earley.token Token_Dot
    expression <- expressionProd
    pure (Expression_Abstraction variable expression)

  applicationProd <- rule "application" do
    _ <- Earley.token Token_OpenParen
    function <- expressionProd
    argument <- expressionProd
    _ <- Earley.token Token_CloseParen
    pure (Expression_Application function argument)

  expressionProd <- rule "expression" do
    asum
      [ Fix <$> Expression_Variable <$> variableProd
      , Fix <$> abstractionProd
      , Fix <$> applicationProd
      ]

  pure expressionProd
