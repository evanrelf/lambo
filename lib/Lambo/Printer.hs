{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambo.Printer
  ( Print
  , print
  , PrintShow (..)
  )
where

import Data.Text (Text)
import Lambo.Lexer (Token (..))
import Lambo.Syntax (Expression, ExpressionF (..))
import Prelude hiding (print)

import qualified Data.Fix as Fix
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


instance Print (Expression Text) where
  print = Fix.foldFix \case
    Expression_Variable name ->
      name
    Expression_Abstraction name body ->
      "λ" <> name <> "." <> body
    Expression_Application function argument ->
      "(" <> function <> " " <> argument <> ")"


instance Print (Expression Int) where
  print = Fix.foldFix \case
    Expression_Variable index ->
      printIndex index
    Expression_Abstraction index body ->
      "λ" <> printIndex index <> "." <> body
    Expression_Application function argument ->
      "(" <> function <> " " <> argument <> ")"
    where
    printIndex index = Text.pack ("#" <> show index)


newtype PrintShow a = PrintShow { unPrintShow :: a }
  deriving newtype Print


instance Print a => Show (PrintShow a) where
  show = Text.unpack . print
