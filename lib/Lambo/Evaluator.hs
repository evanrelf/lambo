{-# LANGUAGE LambdaCase #-}

module Lambo.Evaluator
  ( evaluate
  )
where

import Lambo.Expression (Expression (..))


evaluate :: Expression -> Expression
evaluate = \case
  Expression_Literal literal ->
    undefined

  Expression_Variable name index ->
    undefined

  Expression_Abstraction argument definition ->
    undefined

  Expression_Application function argument ->
    undefined
