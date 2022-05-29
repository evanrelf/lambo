{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Lambo.Expression
  ( Expression (.., (:$), Number),
    Literal (.., Number),
  )
where

import Data.Data (Data)
import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Exts (IsString (..))
import GHC.Generics (Generic)

import qualified Control.Lens as Lens
import qualified Data.Text as Text
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

instance IsString Expression where
  fromString string = Expression_Variable (Text.pack string) 0

data Literal
  = -- | @42@
    Literal_Number Scientific
  deriving stock (Show, Eq, Data, Generic)

instance Lens.Plated Literal

instance Witch.From Literal Expression where
  from = Expression_Literal

pattern (:$) :: Expression -> Expression -> Expression
pattern (:$) f x = Expression_Application f x

pattern Number ::
  (Witch.From Literal a, Witch.From a Expression) => Scientific -> a
pattern Number x <-
  (Witch.into -> Expression_Literal (Literal_Number x))
  where
    Number x = Witch.from (Literal_Number x)
