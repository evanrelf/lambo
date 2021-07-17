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
import Data.Foldable (asum)
import Data.Text (Text)
import Lambo.Lexer (Token (..), lex)
import Lambo.Syntax (Expression (..))
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
  let named = flip (<?>)

  variable <- Earley.rule $ named "variable" do
    Earley.terminal \case
      Token_Variable name -> Just name
      _ -> Nothing

  abstraction <- Earley.rule $ named "abstraction" do
    _ <- Earley.token Token_Lambda
    variable' <- variable
    _ <- Earley.token Token_Dot
    expression' <- expression
    pure (Expression_Abstraction variable' expression')

  application <- Earley.rule $ named "application" do
    _ <- Earley.token Token_OpenParen
    function <- expression
    argument <- expression
    _ <- Earley.token Token_CloseParen
    pure (Expression_Application function argument)

  expression <- Earley.rule $ named "expression" $ asum
    [ Expression_Variable <$> variable
    , abstraction
    , application
    ]

  pure expression
