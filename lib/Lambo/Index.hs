{-# LANGUAGE OverloadedStrings #-}

module Lambo.Index
  ( index
  )
where

import Data.Text (Text)
import Lambo.Syntax (Expression)


index :: Expression Text -> Either Text (Expression Int)
index _ = Left "unimplemented"
