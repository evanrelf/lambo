{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lambo.Syntax
  ( ExpressionF
      ( ..
      , Expression_Variable
      , Expression_Abstraction
      , Expression_Application
      )
  , Expression
  )
where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Text.Show.Deriving (deriveShow1)


-- | @\\f. (\\x. f (x x)) (\\x. f (x x))@
data ExpressionF i a
  = ExpressionF_Variable i
    -- ^ @x@
  | ExpressionF_Abstraction i a
    -- ^ @\\ \<variable\> . \<expression\>@
  | ExpressionF_Application a a
    -- ^ @( \<expression\> \<expression\> )@
  deriving stock (Show, Eq, Functor)

deriveShow1 ''ExpressionF
deriveEq1 ''ExpressionF


type Expression i = Fix (ExpressionF i)


pattern Expression_Variable :: e ~ Expression i => i -> e
pattern Expression_Variable i =
  Fix (ExpressionF_Variable i)


pattern Expression_Abstraction :: e ~ Expression i => i -> e -> e
pattern Expression_Abstraction argument definition =
  Fix (ExpressionF_Abstraction argument definition)


pattern Expression_Application :: e ~ Expression i => e -> e -> e
pattern Expression_Application function argument =
  Fix (ExpressionF_Application function argument)


{-# COMPLETE Expression_Variable, Expression_Abstraction, Expression_Application :: Expression #-}
