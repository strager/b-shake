module B.Shake.Core.Log
  ( Verbosity(..)

  , getVerbosity
  , putLoud
  , putNormal
  , putQuiet
  , quietly
  , traced
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.IO (hPutStrLn, stderr)

import B.Shake.Core.Action (Action)
import B.Shake.Types (Verbosity(..))

getVerbosity :: Action Verbosity
getVerbosity = return Loud

putAs :: Verbosity -> String -> Action ()
putAs verbosity message = do
  maxVerbosity <- getVerbosity
  when (verbosity <= maxVerbosity)
    . liftIO $ hPutStrLn stderr message

putLoud, putNormal, putQuiet :: String -> Action ()
putLoud = putAs Loud
putNormal = putAs Normal
putQuiet = putAs Quiet

quietly :: Action a -> Action a
quietly = error "Unimplemented: B.Shake.Log.quietly"

traced :: String -> IO a -> Action a
traced = error "Unimplemented: B.Shake.Log.traced"
