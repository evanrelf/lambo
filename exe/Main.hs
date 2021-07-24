module Main (main) where

import qualified Lambo
import qualified Options.Applicative as Options


main :: IO ()
main = do
  Options{} <- getOptions

  Lambo.repl


data Options = Options {}


getOptions :: IO Options
getOptions = do
  let parserPrefs = Options.prefs Options.showHelpOnError
  let parserInfo = Options.info (Options.helper <*> parseOptions) mempty
  Options.customExecParser parserPrefs parserInfo


parseOptions :: Options.Parser Options
parseOptions = do
  pure Options{}
