{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Function ((&))
import Data.Text (Text)
import Lambo.Evaluator (evaluate)
import Lambo.Expression (Expression (..))
import Lambo.Lexer (Token (..), lex)
import Lambo.Parser (parse)
import Lambo.QuasiQuoters (parsed)
import Test.Tasty.HUnit ((@?=))
import Prelude hiding (lex)

import qualified Data.Text as Text
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit


main :: IO ()
main = Tasty.defaultMain $ Tasty.testGroup "lambo"
  [ test_lexer
  , test_parser
  , test_evaluator
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
      , Token_Identifier "x"
      , Token_Dot
      , Token_Identifier "x"
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
      , Token_Identifier "x"
      , Token_Identifier "y"
      , Token_Dot
      , Token_Identifier "x"
      ]

  , allEqual
      ["\\foo bar. baz"]
      [ Token_Lambda
      , Token_Identifier "foo"
      , Token_Identifier "bar"
      , Token_Dot
      , Token_Identifier "baz"
      ]

  , allEqual
      ["\\f. (\\x. f (x x)) (\\x. f (x x))"]
      [ Token_Lambda
      , Token_Identifier "f"
      , Token_Dot
      , Token_OpenParen
      , Token_Lambda
      , Token_Identifier "x"
      , Token_Dot
      , Token_Identifier "f"
      , Token_OpenParen
      , Token_Identifier "x"
      , Token_Identifier "x"
      , Token_CloseParen
      , Token_CloseParen
      , Token_OpenParen
      , Token_Lambda
      , Token_Identifier "x"
      , Token_Dot
      , Token_Identifier "f"
      , Token_OpenParen
      , Token_Identifier "x"
      , Token_Identifier "x"
      , Token_CloseParen
      , Token_CloseParen
      ]

  , allEqual
      ["λf.λx.λy.((f y) x)"]
      [ Token_Lambda
      , Token_Identifier "f"
      , Token_Dot
      , Token_Lambda
      , Token_Identifier "x"
      , Token_Dot
      , Token_Lambda
      , Token_Identifier "y"
      , Token_Dot
      , Token_OpenParen
      , Token_OpenParen
      , Token_Identifier "f"
      , Token_Identifier "y"
      , Token_CloseParen
      , Token_Identifier "x"
      , Token_CloseParen
      ]
  ]
  where
  allEqual :: [Text] -> [Token] -> [Tasty.TestTree]
  allEqual inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (lex input @?= Right output)


test_parser :: Tasty.TestTree
test_parser = Tasty.testGroup "Parser" $ mconcat
  [ allEqual
      ["x"]
      (Expression_Variable "x" 0)

  , allEqual
      [ "(f x)"
      , "f x"
      ]
      (Expression_Application
        (Expression_Variable "f" 0)
        (Expression_Variable "x" 0))

  , allEqual
      [ "λx.x"
      , "λx.x@0"
      , "(λx.x)"
      , "((λx.x))"
      , "((λx.(x)))"
      , "((λx.((x))))"
      ]
      (Expression_Abstraction
        "x"
        (Expression_Variable "x" 0))

  , allEqual
      [ "λx.x@42"
      , "λx . x@42"
      ]
      (Expression_Abstraction
        "x"
        (Expression_Variable "x" 42))

  , allEqual
    [ "(((f x) y) z)"
    , "f x y z"
    ]
    (Expression_Application
      (Expression_Application
        (Expression_Application
          (Expression_Variable "f" 0)
          (Expression_Variable "x" 0))
        (Expression_Variable "y" 0))
      (Expression_Variable "z" 0))

  , allEqual
      [ "λf.λx.λy.((f y) x)"
      , "λf. λx. λy. f y x"
      ]
      (Expression_Abstraction
        "f"
        (Expression_Abstraction
          "x"
          (Expression_Abstraction
            "y"
            (Expression_Application
              (Expression_Application
                (Expression_Variable "f" 0)
                (Expression_Variable "y" 0))
              (Expression_Variable "x" 0)))))

  , allEqual
    ["λf. (λx. f (x x)) (λx. f (x x))"]
    (Expression_Abstraction
      "f"
      (Expression_Application
        (Expression_Abstraction
          "x"
          (Expression_Application
            (Expression_Variable "f" 0)
            (Expression_Application
              (Expression_Variable "x" 0)
              (Expression_Variable "x" 0))))
        (Expression_Abstraction
          "x"
          (Expression_Application
            (Expression_Variable "f" 0)
            (Expression_Application
              (Expression_Variable "x" 0)
              (Expression_Variable "x" 0))))))
  ]
  where
  allEqual :: [Text] -> Expression -> [Tasty.TestTree]
  allEqual inputs output = inputs & fmap \input ->
    HUnit.testCase (Text.unpack input) (parse input @?= Right output)


test_evaluator :: Tasty.TestTree
test_evaluator = Tasty.testGroup "Evaluator"
  [ HUnit.testCase "identity" do
      evaluate [parsed|(\x.x)|] @?= [parsed|(\x.x)|]

  , HUnit.testCase "arithmetic" do
      evaluate [parsed|add 2 2|] @?= [parsed|4|]
      evaluate [parsed|sub 2 2|] @?= [parsed|0|]
      evaluate [parsed|mul 5 2|] @?= [parsed|10|]
      evaluate [parsed|div 9 2|] @?= [parsed|4.5|]

  , HUnit.testCase "recursive rewriting" do
      evaluate [parsed|add (add 1 1) (add 1 1)|] @?= [parsed|4|]
      evaluate [parsed|foo (add 1 1) (add 1 1)|] @?= [parsed|foo 2 2|]
  ]
