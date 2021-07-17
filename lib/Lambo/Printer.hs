{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambo.Printer
  ( Print
  , print
  )
where

import Data.Text (Text)
import Lambo.Lexer (Token (..))
import Lambo.Syntax (Expression (..))
import Prelude hiding (print)

import qualified Data.Text as Text


class Print a where
  print :: a -> Text


instance Print Token where
  print = \case
    Token_Lambda -> "λ"
    Token_Dot -> "."
    Token_Variable name -> name
    Token_OpenParen -> "("
    Token_CloseParen -> ")"


instance Print [Token] where
  print = Text.unwords . fmap print


instance Print Expression where
  print = \case
    Expression_Variable name -> name
    Expression_Abstraction variable body ->
      "λ" <> variable <> "." <> print body
    Expression_Application function argument ->
      "(" <> print function <> " " <> print argument <> ")"
