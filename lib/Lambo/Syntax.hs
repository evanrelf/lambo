{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}

module Lambo.Syntax
  ( Expression
  , ExpressionF (..)
  )
where

import Data.Fix (Fix)
import Data.Text (Text)


type Expression = Fix ExpressionF


-- | @\\f. (\\x. f (x x)) (\\x. f (x x))@
data ExpressionF a
  = Expression_Variable Text
    -- ^ @x@
  | Expression_Abstraction Text a
    -- ^ @\\ \<variable\> . \<expression\>@
  | Expression_Application a a
    -- ^ @( \<expression\> \<expression\> )@
  deriving stock (Show, Eq, Functor)
