{-# LANGUAGE LambdaCase #-}

module Lambo.Evaluator
  ( evaluate
  )
where

import Lambo.Expression (Expression (..))


evaluate :: Expression -> Expression
evaluate = \case
  Expression_Literal literal ->
    Expression_Literal literal

  Expression_Variable name index ->
    Expression_Variable name index

  Expression_Abstraction argument definition ->
    Expression_Abstraction argument (evaluate definition)

  Expression_Application function argument ->
    Expression_Application (evaluate function) (evaluate argument)
