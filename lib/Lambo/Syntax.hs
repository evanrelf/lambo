{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
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

import Data.Data (Data)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Text (Text)
import Text.Show.Deriving (deriveShow1)


-- | @λf. (λx. f (x x)) (λx. f (x x))@
data ExpressionF a
  = ExpressionF_Variable Text
    -- ^ @x@
  | ExpressionF_Abstraction Text a
    -- ^ @λ \<variable\> . \<expression\>@
  | ExpressionF_Application a a
    -- ^ @( \<expression\> \<expression\> )@
  deriving stock (Show, Eq, Functor, Foldable, Traversable, Data)

deriveShow1 ''ExpressionF
deriveEq1 ''ExpressionF


type Expression = Fix ExpressionF


pattern Expression_Variable :: e ~ Expression => Text -> e
pattern Expression_Variable name =
  Fix (ExpressionF_Variable name)


pattern Expression_Abstraction :: e ~ Expression => Text -> e -> e
pattern Expression_Abstraction argument definition =
  Fix (ExpressionF_Abstraction argument definition)


pattern Expression_Application :: e ~ Expression => e -> e -> e
pattern Expression_Application function argument =
  Fix (ExpressionF_Application function argument)


{-# COMPLETE Expression_Variable, Expression_Abstraction, Expression_Application :: Expression #-}
