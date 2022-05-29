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
{-# OPTIONS_GHC -Wno-orphans #-}

module Lambo.Expression.Sugar
  ( Expression (.., Number, (:@), (:.), (:$)),
    Literal (.., Number),
  )
where

import Data.Scientific (Scientific)
import Data.Text (Text)
import GHC.Exts (IsString (..))
import Lambo.Expression (Expression (..), Literal (..))

import qualified Data.Text as Text
import qualified Witch

pattern Number ::
  (Witch.From Literal a, Witch.From a Expression) => Scientific -> a
pattern Number x <-
  (Witch.into -> Expression_Literal (Literal_Number x))
  where
    Number x = Witch.from (Literal_Number x)

instance IsString Expression where
  fromString string = Expression_Variable (Text.pack string) 0

pattern (:@) :: Text -> Int -> Expression
pattern (:@) name index = Expression_Variable name index

pattern (:.) :: Text -> Expression -> Expression
pattern (:.) name argument = Expression_Abstraction name argument

pattern (:$) :: Expression -> Expression -> Expression
pattern (:$) left right = Expression_Application left right
