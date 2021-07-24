{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module Lambo.Syntax
  ( Expression (..)
  )
where

import Data.Data (Data)
import Data.Text (Text)


-- | @λf. (λx. f (x x)) (λx. f (x x))@
data Expression
  = Expression_Variable Text Int
    -- ^ @x@
  | Expression_Abstraction Text Expression
    -- ^ @λ \<variable\> . \<expression\>@
  | Expression_Application Expression Expression
    -- ^ @( \<expression\> \<expression\> )@
  deriving stock (Show, Eq, Data)
