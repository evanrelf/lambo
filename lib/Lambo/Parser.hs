{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecursiveDo #-}

module Lambo.Parser
  ( parse
  , parseTokens
  )
where

import Control.Monad ((>=>))
import Data.Text (Text)
import Lambo.Lexer (Token (..), lex)
import Lambo.Syntax (Expression (..))
import Prelude hiding (lex)

import qualified Text.Earley as Earley


type Production r = Earley.Prod r Text Token


parse :: Text -> Either Text Expression
parse = lex >=> parseTokens


parseTokens :: [Token] -> Either Text Expression
parseTokens tokens =
  case Earley.fullParses (Earley.parser grammar) tokens of
    ([], _report) ->
      -- TODO
      undefined

    (result : _, _) ->
      pure result


grammar :: Earley.Grammar r (Production r Expression)
grammar = mdo
  undefined
