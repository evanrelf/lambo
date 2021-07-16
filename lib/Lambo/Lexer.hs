module Lambo.Lexer
  ( Token (..)
  )
where

import Data.Text (Text)


data Token
  = Token_Lambda
    -- ^ \
  | Token_Dot
    -- ^ .
  | Token_Variable Text
    -- ^ x
  | Token_OpenParen
    -- ^ (
  | Token_CloseParen
    -- ^ )
