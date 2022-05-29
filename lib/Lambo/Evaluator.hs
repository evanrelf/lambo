{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lambo.Evaluator
  ( evaluate,
  )
where

import Lambo.Expression (Expression (..))

import qualified Control.Lens as Lens

evaluate :: Expression -> Expression
evaluate = Lens.transform arithmetic

arithmetic :: Expression -> Expression
arithmetic = \case
  Expression_Variable function _ :$ Number x :$ Number y
    | function == "add" -> Number (x + y)
    | function == "sub" -> Number (x - y)
    | function == "mul" -> Number (x * y)
    | function == "div" -> Number (x / y)
  e -> e
