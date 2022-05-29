{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambo.Printer
  ( Print,
    print,
    PrintShow (..),
  )
where

import Data.Text (Text)
import Lambo.Expression (Expression (..), Literal (..))
import Lambo.Lexer (Token (..))
import Prelude hiding (print)

import qualified Data.Text as Text

class Print a where
  print :: a -> Text

instance Print Token where
  print = \case
    Token_Lambda -> "λ"
    Token_Dot -> "."
    Token_Identifier name -> name
    Token_At -> "@"
    Token_Dash -> "-"
    Token_Decimal number -> Text.pack (show number)
    Token_OpenParen -> "("
    Token_CloseParen -> ")"

instance Print [Token] where
  print = Text.unwords . fmap print

instance Print Literal where
  print = \case
    Literal_Number n ->
      Text.pack (show n)

instance Print Expression where
  print = \case
    Expression_Literal literal ->
      print literal
    Expression_Variable name 0 ->
      name
    Expression_Variable name index ->
      name <> "@" <> Text.pack (show index)
    Expression_Abstraction argument definition ->
      "(λ" <> argument <> "." <> print definition <> ")"
    Expression_Application function argument ->
      "(" <> print function <> " " <> print argument <> ")"

newtype PrintShow a = PrintShow {unPrintShow :: a}
  deriving newtype (Print)

instance Print a => Show (PrintShow a) where
  show = Text.unpack . print
