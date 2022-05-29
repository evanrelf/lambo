{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Lambo.Evaluator
  ( evaluate,
  )
where

import Lambo.Expression.Sugar (Expression (..))

import qualified Control.Lens as Lens

evaluate :: Expression -> Expression
evaluate = Lens.transform arithmetic

arithmetic :: Expression -> Expression
arithmetic = \case
  "add" :$ Number x :$ Number y -> Number (x + y)
  "sub" :$ Number x :$ Number y -> Number (x - y)
  "mul" :$ Number x :$ Number y -> Number (x * y)
  "div" :$ Number x :$ Number y -> Number (x / y)
  e -> e
