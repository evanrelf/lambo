{-# LANGUAGE NamedFieldPuns #-}

module Lambo.QuasiQuoter
  ( lambo
  )
where

import Language.Haskell.TH.Quote (QuasiQuoter (..))


lambo :: QuasiQuoter
lambo = QuasiQuoter{quoteExp, quotePat, quoteType, quoteDec}
  where
  quoteExp = unsupported "expression"

  quotePat = unsupported "pattern"

  quoteType = unsupported "type"

  quoteDec = unsupported "declaration"

  unsupported context _ = fail $ unwords
    [ "Unsupported operation: this QuasiQuoter cannot be used in a "
    , context <> "context"
    ]
