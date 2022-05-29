{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Lambo.Repl
  ( repl,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor ((<&>))
import Data.Text (Text)
import Lambo.Evaluator (evaluate)
import Lambo.Lexer (lex)
import Lambo.Parser (parse)
import Lambo.Printer (print)
import System.Console.Repline (ReplOpts (..))
import Text.Pretty.Simple (pPrintNoColor)
import Prelude hiding (lex, print)

import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Console.Repline as Repline
import qualified System.IO as IO

repl :: MonadIO m => m ()
repl = liftIO do
  Repline.evalReplOpts
    ReplOpts
      { banner = \_ -> pure "lambo> "
      , command = evalCommand
      , options =
          [ ("lex", lexCommand)
          , ("parse", parseCommand)
          , ("eval", evalCommand)
          ]
      , prefix = Just ':'
      , multilineCommand = Nothing
      , tabComplete = Repline.File
      , initialiser = pure ()
      , finaliser = pure Repline.Exit
      }

lexCommand :: MonadIO m => String -> m ()
lexCommand string = liftIO do
  lex (Text.pack string) `dischargeError` \tokens -> do
    Text.putStrLn (print tokens)
    pPrintNoColor tokens

parseCommand :: MonadIO m => String -> m ()
parseCommand string = liftIO do
  parse (Text.pack string) `dischargeError` \expression -> do
    Text.putStrLn (print expression)
    pPrintNoColor expression

evalCommand :: MonadIO m => String -> m ()
evalCommand string = liftIO do
  (parse (Text.pack string) <&> evaluate) `dischargeError` \expression -> do
    Text.putStrLn (print expression)

dischargeError :: MonadIO m => Either Text a -> (a -> m ()) -> m ()
dischargeError e k =
  case e of
    Left err -> liftIO $ Text.hPutStrLn IO.stderr err
    Right x -> k x
