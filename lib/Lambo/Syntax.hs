{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Lambo.Syntax
  ( Expression
  , ExpressionF (..)
  )
where

import Data.Fix (Fix)


type Expression i = Fix (ExpressionF i)


-- | @\\f. (\\x. f (x x)) (\\x. f (x x))@
data ExpressionF i a
  = Expression_Variable i
    -- ^ @x@
  | Expression_Abstraction i a
    -- ^ @\\ \<variable\> . \<expression\>@
  | Expression_Application a a
    -- ^ @( \<expression\> \<expression\> )@
  deriving stock (Show, Eq, Functor)
