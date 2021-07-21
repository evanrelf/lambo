{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Lambo.Lexer
  ( Token (..)
  , lex
  )
where

import Control.Applicative (empty, (<|>))
import Data.Data (Data)
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Void (Void)
import Prelude hiding (lex)

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Char as Megaparsec
import qualified Text.Megaparsec.Char.Lexer as Megaparsec.Lexer


data Token
  = Token_Lambda
    -- ^ @\\@
  | Token_Dot
    -- ^ @.@
  | Token_Variable Text
    -- ^ @x@
  | Token_OpenParen
    -- ^ @(@
  | Token_CloseParen
    -- ^ @)@
  deriving stock (Show, Eq, Data)


type Parser = Megaparsec.Parsec Void Text


lex :: Text -> Either Text [Token]
lex source =
  case Megaparsec.parse parseTokens "<input>" source of
    Left parseErrorBundle ->
      Left $ Text.pack (Megaparsec.errorBundlePretty parseErrorBundle)
    Right tokens ->
      Right tokens


parseTokens :: Parser [Token]
parseTokens = parseSpace *> Megaparsec.manyTill parseToken Megaparsec.eof


parseToken :: Parser Token
parseToken = asum
  [ Token_Lambda
      <$ (parseSymbol "λ" <|> parseSymbol "\\")
  , Token_Dot
      <$ (parseSymbol "." <|> parseSymbol "->")
  , Token_Variable
      <$> parseVariable
  , Token_OpenParen
      <$ parseSymbol "("
  , Token_CloseParen
      <$ parseSymbol ")"
  ]


parseVariable :: Parser Text
parseVariable = parseLexeme do
  c <- Megaparsec.satisfy \char -> any ($ char)
    [ Char.isAsciiLower
    , (==) '_'
    ]
  cs <- Megaparsec.takeWhileP Nothing \char -> any ($ char)
    [ Char.isAsciiLower
    , Char.isAsciiUpper
    , Char.isDigit
    , (==) '_'
    , (==) '\''
    ]
  pure (c `Text.cons` cs)


parseLexeme :: Parser a -> Parser a
parseLexeme = Megaparsec.Lexer.lexeme parseSpace


parseSymbol :: Text -> Parser Text
parseSymbol = Megaparsec.Lexer.symbol parseSpace


parseSpace :: Parser ()
parseSpace =
  Megaparsec.Lexer.space
    Megaparsec.space1
    (   Megaparsec.Lexer.skipLineComment "--"
    <|> Megaparsec.Lexer.skipLineComment "#"
    )
    empty
