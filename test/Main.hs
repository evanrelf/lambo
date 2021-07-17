{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
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
  [ successful
      [ "\\x.x"
      , "λx.x"
      , "\\x->x"
      , "\\ x . x"
      , "λ x . x"
      , "\\ x -> x"
      , "  \\x.x  "
      ]
      [ Token_Lambda
      , Token_Variable "x"
      , Token_Dot
      , Token_Variable "x"
      ]

  , successful
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

  , successful
      ["\\foo bar. baz"]
      [ Token_Lambda
      , Token_Variable "foo"
      , Token_Variable "bar"
      , Token_Dot
      , Token_Variable "baz"
      ]

  , successful
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

  , successful
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
  successful :: [Text] -> [Token] -> [Tasty.TestTree]
  successful inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (lex input @=? Right output)

test_parser :: Tasty.TestTree
test_parser = Tasty.testGroup "Parser" $ mconcat
  [ successful
      ["x"]
      (Expression_Variable "x")

  , successful
      ["(f x)"]
      (Expression_Application
        (Expression_Variable "f")
        (Expression_Variable "x"))

  , successful
      ["λx.x"]
      (Expression_Abstraction
        ["x"]
        (Expression_Variable "x"))

  , successful
      ["λf.λx.λy.((f y) x)"]
      (Expression_Abstraction
        ["f"]
        (Expression_Abstraction
          ["x"]
          (Expression_Abstraction
            ["y"]
            (Expression_Application
              (Expression_Application
                (Expression_Variable "f")
                (Expression_Variable "y"))
              (Expression_Variable "x")))))

  , successful
      ["λf x y.((f y) x)"]
      (Expression_Abstraction
        ["f", "x", "y"]
        (Expression_Application
          (Expression_Application
            (Expression_Variable "f")
            (Expression_Variable "y"))
          (Expression_Variable "x")))
  ]
  where
  successful :: [Text] -> Expression -> [Tasty.TestTree]
  successful inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (parse input @=? Right output)
