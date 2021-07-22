{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambo.Index
  ( index
  )
where

import Data.Fix (Fix (..))
import Data.Text (Text)
import Lambo.Syntax (Expression, ExpressionF (..))

import qualified Data.List as List


index :: Expression Text -> Either Text (Expression Int)
index = go []
  where
  go names = \case
    Fix (ExpressionF_Variable name) ->
      case List.elemIndex name names of
        Nothing -> Left ("Unknown variable: " <> name)
        Just i -> Right (Expression_Variable (i + 1))

    Fix (ExpressionF_Abstraction argument definition) ->
      Expression_Abstraction
        <$> pure 1
        <*> go (argument : names) definition

    Fix (ExpressionF_Application function argument) ->
      Expression_Application
        <$> go names function
        <*> go names argument
