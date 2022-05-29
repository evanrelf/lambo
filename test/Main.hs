{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Function ((&))
import Data.Text (Text)
import Lambo.Expression (Expression (..))
import Lambo.Lexer (Token (..), lex)
import Lambo.Parser (parse)
import Lambo.QuasiQuoters (evaluated, parsed)
import Test.Tasty.HUnit ((@?=))
import Prelude hiding (lex)

import qualified Data.Text as Text
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main = Tasty.defaultMain do
  Tasty.testGroup "lambo" $
    [ test_lexer
    , test_parser
    , test_evaluator
    ]

test_lexer :: Tasty.TestTree
test_lexer = Tasty.testGroup "Lexer" do
  mconcat
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
    allEqual inputs output =
      inputs & fmap \input ->
        HUnit.testCase (Text.unpack input) (lex input @?= Right output)

test_parser :: Tasty.TestTree
test_parser = Tasty.testGroup "Parser" do
  mconcat
    [ allEqual
        ["x"]
        (Expression_Variable "x" 0)
    , allEqual
        [ "(f x)"
        , "f x"
        ]
        ("f" :$ "x")
    , allEqual
        [ "λx.x"
        , "λx.x@0"
        , "(λx.x)"
        , "((λx.x))"
        , "((λx.(x)))"
        , "((λx.((x))))"
        ]
        ("x" :. "x")
    , allEqual
        [ "λx.x@42"
        , "λx . x@42"
        ]
        ("x" :. ("x" :@ 42))
    , allEqual
        [ "(((f x) y) z)"
        , "f x y z"
        ]
        ("f" :$ "x" :$ "y" :$ "z")
    , allEqual
        [ "λf.λx.λy.((f y) x)"
        , "λf. λx. λy. f y x"
        ]
        ("f" :. ("x" :. ("y" :. ("f" :$ "y" :$ "x"))))
    , allEqual
        ["λf. (λx. f (x x)) (λx. f (x x))"]
        ("f" :. (("x" :. ("f" :$ ("x" :$ "x"))) :$ ("x" :. ("f" :$ ("x" :$ "x")))))
    ]
  where
    allEqual :: [Text] -> Expression -> [Tasty.TestTree]
    allEqual inputs output =
      inputs & fmap \input ->
        HUnit.testCase (Text.unpack input) (parse input @?= Right output)

test_evaluator :: Tasty.TestTree
test_evaluator =
  Tasty.testGroup "Evaluator" $
    [ HUnit.testCase "identity" do
        [evaluated|(\x.x)|] @?= [parsed|(\x.x)|]
    , HUnit.testCase "arithmetic" do
        [evaluated|add 2 2|] @?= [parsed|4|]
        [evaluated|sub 2 2|] @?= [parsed|0|]
        [evaluated|mul 5 2|] @?= [parsed|10|]
        [evaluated|div 9 2|] @?= [parsed|4.5|]
    , HUnit.testCase "recursive rewriting" do
        [evaluated|add (add 1 1) (add 1 1)|] @?= [parsed|4|]
        [evaluated|foo (add 1 1) (add 1 1)|] @?= [parsed|foo 2 2|]
    ]
