{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main (main) where

import Lambo.Lexer (lex)
import Text.Read (readMaybe)
import Prelude hiding (lex)

import qualified Data.Char as Char
import qualified Data.Text.IO as Text
import qualified Options.Applicative as Options


main :: IO ()
main = do
  Options{phase, file} <- getOptions

  case phase of
    Lexer -> do
      source <-
        if file == "-"
          then Text.getContents
          else Text.readFile file

      case lex source of
        Left err -> Text.putStrLn err
        Right tokens -> print tokens


data Options = Options
  { phase :: Phase
  , file :: FilePath
  }


data Phase
  = Lexer
  deriving stock (Show, Read, Enum, Bounded)


getOptions :: IO Options
getOptions = do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseOptions) mempty
  Options.customExecParser parserPrefs parserInfo


parseOptions :: Options.Parser Options
parseOptions = do
  phase <- Options.option phaseOption $ mconcat
    [ Options.long "phase"
    , Options.metavar "PHASE"
    , Options.help $ unwords
        [ "Which phase to stop processing"
        , "(one of 'lexer'; default is 'lexer')"
        ]
    , Options.value maxBound
    , Options.hidden
    ]

  file <- Options.strArgument $ mconcat
    [ Options.metavar "FILE"
    , Options.help "Path to source file (defaults to stdin)"
    , Options.value "-"
    ]

  pure Options{phase, file}
  where
  phaseOption :: Options.ReadM Phase
  phaseOption =
    Options.maybeReader \case
      (s : tring) -> readMaybe (Char.toUpper s : tring)
      _ -> Nothing
