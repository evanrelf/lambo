{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Data.Text (Text)
import Text.Read (readMaybe)

import qualified Data.Char as Char
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

  let output :: Lambo.Print a => Either Text a -> IO ()
      output = \case
        Left err -> do
          Text.hPutStrLn IO.stderr err
          Exit.exitFailure

        Right ok ->
          Text.putStrLn (Lambo.print ok)

  case phase of
    Lexer -> output $ Lambo.lex source
    Parser -> output $ Lambo.parse source


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
