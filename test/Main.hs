{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function ((&))
import Data.Text (Text)
import Lambo.Lexer (Token (..), lex)
import Test.Tasty.HUnit ((@=?))
import Prelude hiding (lex)

import qualified Data.Text as Text
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "lambo"
  [ test_lexer
  ]


test_lexer :: Tasty.TestTree
test_lexer = Tasty.testGroup "Lexer" $ mconcat
  [ successful
      [ "\\x.x"
      , "\\ x . x"
      , "  \\x.x  "
      ]
      [ Token_Lambda
      , Token_Variable "x"
      , Token_Dot
      , Token_Variable "x"
      ]

  , successful
      [ "\\x y. x"
      , "\\x y . x"
      ]
      [ Token_Lambda
      , Token_Variable "x"
      , Token_Variable "y"
      , Token_Dot
      , Token_Variable "x"
      ]

  , successful
      ["\\foo bar. baz"]
      [ Token_Lambda
      , Token_Variable "foo"
      , Token_Variable "bar"
      , Token_Dot
      , Token_Variable "baz"
      ]
  ]
  where
  successful :: [Text] -> [Token] -> [Tasty.TestTree]
  successful inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (lex input @=? Right output)
