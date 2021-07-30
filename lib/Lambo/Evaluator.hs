{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambo.Evaluator
  ( evaluate
  )
where

import Lambo.Expression (Expression (..), Literal (..))

import qualified Control.Lens as Lens


evaluate :: Expression -> Expression
evaluate = Lens.transform arithmetic


arithmetic :: Expression -> Expression
arithmetic = \case
  Expression_Application
    (Expression_Application
      (Expression_Variable function _)
      (Expression_Literal (Literal_Number x)))
    (Expression_Literal (Literal_Number y))
    | function == "add" -> Expression_Literal (Literal_Number (x + y))
    | function == "sub" -> Expression_Literal (Literal_Number (x - y))
    | function == "mul" -> Expression_Literal (Literal_Number (x * y))
    | function == "div" -> Expression_Literal (Literal_Number (x / y))
  e -> e
