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


instance Print Expression where
  print = Fix.foldFix \case
    ExpressionF_Variable name 0 ->
      name
    ExpressionF_Variable name index ->
      name <> "#" <> Text.pack (show index)
    ExpressionF_Abstraction argument definition ->
      "λ" <> argument <> "." <> definition
    ExpressionF_Application function argument ->
      "(" <> function <> " " <> argument <> ")"


newtype PrintShow a = PrintShow { unPrintShow :: a }
  deriving newtype Print


instance Print a => Show (PrintShow a) where
  show = Text.unpack . print
