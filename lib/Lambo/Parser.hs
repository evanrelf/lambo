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

import Control.Applicative (optional)
import Control.Monad ((>=>))
import Data.Foldable (asum)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Lambo.Expression (Expression (..), Literal (..))
import Lambo.Lexer (Token (..), lex)
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

  numberProd <- rule "number" do
    Earley.terminal \case
      Token_Decimal n ->
        Just (Expression_Literal (Literal_Number n))
      _ ->
        Nothing

  literalProd <- rule "literal" $ asum
    [ numberProd
    ]

  variableProd <- rule "variable" do
    name <- identifierProd
    index <- fromMaybe 0 <$> optional do
      _ <- Earley.token Token_At
      index <- Earley.terminal \case
        Token_Decimal number -> Just number
        _ -> Nothing
      pure index
    pure (Expression_Variable name index)

  abstractionProd <- rule "abstraction" do
    _ <- Earley.token Token_Lambda
    argument <- identifierProd
    _ <- Earley.token Token_Dot
    definition <- expressionProd
    pure (Expression_Abstraction argument definition)

  applicationProd <- rule "application" do
    function <- asum
      [ inParens abstractionProd
      , inParens applicationProd
      , applicationProd
      , variableProd
      , literalProd
      ]
    argument <- asum
      [ inParens abstractionProd
      , inParens applicationProd
      , variableProd
      , literalProd
      ]
    pure (Expression_Application function argument)

  expressionProd <- rule "expression" do
    asum
      [ abstractionProd
      , applicationProd
      , variableProd
      , literalProd
      , inParens expressionProd
      ]

  pure expressionProd
