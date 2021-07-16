module Lambo.Lexer
  ( Token (..)
  , lex
  )
where

import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (lex)

import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec


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


type Parser = Megaparsec.Parsec Void Text


lex :: Text -> Either Text [Token]
lex source =
  case Megaparsec.parse parseTokens "<input>" source of
    Left parseErrorBundle ->
      Left $ Text.pack (Megaparsec.errorBundlePretty parseErrorBundle)
    Right tokens ->
      Right tokens


parseTokens :: Parser [Token]
parseTokens = undefined
