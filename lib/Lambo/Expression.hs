{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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
import qualified Witch

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

instance Witch.From Literal Expression where
  from = Expression_Literal
