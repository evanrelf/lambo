module Lambo.Repl
  ( repl
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import System.Console.Repline (ReplOpts (..))

import qualified System.Console.Repline as Repline


repl :: MonadIO m => m ()
repl = liftIO $ Repline.evalReplOpts ReplOpts
  { banner = \_ -> pure "lambo> "
  , command = \_ -> pure ()
  , options = []
  , prefix = Just ':'
  , multilineCommand = Nothing
  , tabComplete = Repline.File
  , initialiser = pure ()
  , finaliser = pure Repline.Exit
  }
