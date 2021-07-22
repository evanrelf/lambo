{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import Text.Pretty.Simple (pPrintNoColor)
import Text.Read (readMaybe)

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Lambo
import qualified Options.Applicative as Options
import qualified System.Exit as Exit
import qualified System.IO as IO


main :: IO ()
main = do
  Options{file, phase} <- getOptions

  source <-
    if file == "-"
      then Text.getContents
      else Text.readFile file

  Text.putStrLn "INPUT"
  Text.putStrLn (Text.strip source)
  Text.putStr "\n"

  case phase of
    Lexer -> do
      Text.putStrLn "LEXING"
      tokens <- unwrap $ Lambo.lex source
      pPrintNoColor tokens
      Text.putStr "\n"

      Text.putStrLn "PRINTING"
      printIO tokens

    Parser -> do
      Text.putStrLn "LEXING"
      tokens <- unwrap $ Lambo.lex source
      printIO tokens
      pPrintNoColor tokens
      Text.putStr "\n"

      Text.putStrLn "PARSING"
      namedExpression <- unwrap $ Lambo.parseTokens tokens
      pPrintNoColor namedExpression
      printIO namedExpression
      Text.putStr "\n"

      Text.putStrLn "INDEXING"
      indexedExpression <- unwrap $ Lambo.index namedExpression
      pPrintNoColor indexedExpression
      printIO indexedExpression

  where
  unwrap :: Either Text a -> IO a
  unwrap = \case
    Left err -> do
      Text.putStrLn "FAILED"
      Text.hPutStrLn IO.stderr err
      Exit.exitFailure

    Right ok ->
      pure ok

  printIO :: Lambo.Print a => a -> IO ()
  printIO = Text.putStrLn . Lambo.print


data Options = Options
  { file :: FilePath
  , phase :: Phase
  }


data Phase
  = Lexer
  | Parser
  deriving stock (Show, Read, Enum, Bounded)


getOptions :: IO Options
getOptions = do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseOptions) mempty
  Options.customExecParser parserPrefs parserInfo


parseOptions :: Options.Parser Options
parseOptions = do
  file <- Options.strArgument $ mconcat
    [ Options.metavar "FILE"
    , Options.help "Path to source file (defaults to stdin)"
    , Options.value "-"
    ]

  phase <- Options.option phaseOption $ mconcat
    [ Options.long "phase"
    , Options.metavar "PHASE"
    , Options.help $ unwords
        [ "Which phase to stop processing"
        , "(one of 'lexer', 'parser'; default is 'parser')"
        ]
    , Options.value maxBound
    , Options.hidden
    ]

  pure Options{file, phase}
  where
  phaseOption :: Options.ReadM Phase
  phaseOption =
    Options.maybeReader \case
      (s : tring) -> readMaybe (Char.toUpper s : tring)
      _ -> Nothing
