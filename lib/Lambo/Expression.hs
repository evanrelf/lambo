{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Lambo.Expression
  ( Expression (..),
    Literal (..),
  )
where

import Data.Data (Data)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Control.Lens as Lens

-- | @λf. (λx. f (x x)) (λx. f (x x))@
data Expression
  = -- | @42@
    Expression_Literal Literal
  | -- | @x@
    Expression_Variable Text Int
  | -- | @λ \<variable\> . \<expression\>@
    Expression_Abstraction Text Expression
  | -- | @( \<expression\> \<expression\> )@
    Expression_Application Expression Expression
  deriving stock (Show, Eq, Data, Generic)

instance Lens.Plated Expression

data Literal
  = -- | @42@
    Literal_Number Scientific
  deriving stock (Show, Eq, Data, Generic)

instance Lens.Plated Literal
