{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Lambo.QuasiQuoters
  ( lexed
  , parsed
  )
where

import Data.Data (Data)
import Data.Text (Text)
import Lambo.Lexer (lex)
import Lambo.Parser (parse)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Prelude hiding (lex)

import qualified Data.Data as Data
import qualified Data.Text as Text
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.TH.Syntax as TH


lexed :: QuasiQuoter
lexed = qqFrom lex


parsed :: QuasiQuoter
parsed = qqFrom parse


qqFrom :: Data a => (Text -> Either Text a) -> QuasiQuoter
qqFrom parser = QuasiQuoter{quoteExp, quotePat, quoteType, quoteDec}
  where
  quoteExp string =
    case parser (Text.pack string) of
      Left err -> fail (Text.unpack err)
      Right x -> liftDataWithText x

  quotePat = unsupported "pattern"

  quoteType = unsupported "type"

  quoteDec = unsupported "declaration"

  unsupported context _ = fail $ unwords
    [ "Unsupported operation: this QuasiQuoter cannot be used in a "
    , context <> "context"
    ]

  -- https://stackoverflow.com/q/38143464
  liftDataWithText :: Data a => a -> TH.Q TH.Exp
  liftDataWithText = TH.dataToExpQ \x -> liftText <$> Data.cast x

  liftText :: Text -> TH.Q TH.Exp
  liftText text = TH.AppE (TH.VarE 'Text.pack) <$> TH.lift (Text.unpack text)
