{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}

module Lambo.Expression
  ( Expression (..)
  , Literal (..)
  )
where

import Data.Data (Data)
import Data.Scientific (Scientific)
import Data.Text (Text)


-- | @λf. (λx. f (x x)) (λx. f (x x))@
data Expression
  = Expression_Literal Literal
    -- ^ @42@
  | Expression_Variable Text Int
    -- ^ @x@
  | Expression_Abstraction Text Expression
    -- ^ @λ \<variable\> . \<expression\>@
  | Expression_Application Expression Expression
    -- ^ @( \<expression\> \<expression\> )@
  deriving stock (Show, Eq, Data)


data Literal
  = Literal_Number Scientific
    -- ^ @42@
  deriving stock (Show, Eq, Data)
