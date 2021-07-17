{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Function ((&))
import Data.Text (Text)
import Lambo.Lexer (Token (..), lex)
import Lambo.Parser (parse)
import Lambo.Syntax (Expression (..))
import Test.Tasty.HUnit ((@=?))
import Prelude hiding (lex)

import qualified Data.Text as Text
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "lambo"
  [ test_lexer
  , test_parser
  ]


test_lexer :: Tasty.TestTree
test_lexer = Tasty.testGroup "Lexer" $ mconcat
  [ allEqual
      [ "\\x.x"
      , "λx.x"
      , "\\x->x"
      , "\\ x . x"
      , "λ x . x"
      , "\\ x -> x"
      , "  \\x.x  "
      , "-- foo\nλx.x"
      , "# foo\nλx.x"
      ]
      [ Token_Lambda
      , Token_Variable "x"
      , Token_Dot
      , Token_Variable "x"
      ]

  , allEqual
      [ "\\x y-> x"
      , "\\x y -> x"
      , "\\x y ->x"
      , "λx y. x"
      , "λx y . x"
      , "λx y .x"
      ]
      [ Token_Lambda
      , Token_Variable "x"
      , Token_Variable "y"
      , Token_Dot
      , Token_Variable "x"
      ]

  , allEqual
      ["\\foo bar. baz"]
      [ Token_Lambda
      , Token_Variable "foo"
      , Token_Variable "bar"
      , Token_Dot
      , Token_Variable "baz"
      ]

  , allEqual
      ["\\f. (\\x. f (x x)) (\\x. f (x x))"]
      [ Token_Lambda
      , Token_Variable "f"
      , Token_Dot
      , Token_OpenParen
      , Token_Lambda
      , Token_Variable "x"
      , Token_Dot
      , Token_Variable "f"
      , Token_OpenParen
      , Token_Variable "x"
      , Token_Variable "x"
      , Token_CloseParen
      , Token_CloseParen
      , Token_OpenParen
      , Token_Lambda
      , Token_Variable "x"
      , Token_Dot
      , Token_Variable "f"
      , Token_OpenParen
      , Token_Variable "x"
      , Token_Variable "x"
      , Token_CloseParen
      , Token_CloseParen
      ]

  , allEqual
      ["λf.λx.λy.((f y) x)"]
      [ Token_Lambda
      , Token_Variable "f"
      , Token_Dot
      , Token_Lambda
      , Token_Variable "x"
      , Token_Dot
      , Token_Lambda
      , Token_Variable "y"
      , Token_Dot
      , Token_OpenParen
      , Token_OpenParen
      , Token_Variable "f"
      , Token_Variable "y"
      , Token_CloseParen
      , Token_Variable "x"
      , Token_CloseParen
      ]
  ]
  where
  allEqual :: [Text] -> [Token] -> [Tasty.TestTree]
  allEqual inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (lex input @=? Right output)

test_parser :: Tasty.TestTree
test_parser = Tasty.testGroup "Parser" $ mconcat
  [ allEqual
      ["x"]
      (Expression_Variable "x")

  , allEqual
      ["(f x)"]
      (Expression_Application
        (Expression_Variable "f")
        (Expression_Variable "x"))

  , allEqual
      ["λx.x"]
      (Expression_Abstraction
        "x"
        (Expression_Variable "x"))

  , allEqual
      ["λf.λx.λy.((f y) x)"]
      (Expression_Abstraction
        "f"
        (Expression_Abstraction
          "x"
          (Expression_Abstraction
            "y"
            (Expression_Application
              (Expression_Application
                (Expression_Variable "f")
                (Expression_Variable "y"))
              (Expression_Variable "x")))))
  ]
  where
  allEqual :: [Text] -> Expression -> [Tasty.TestTree]
  allEqual inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (parse input @=? Right output)
