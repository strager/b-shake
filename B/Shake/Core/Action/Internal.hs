{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module B.Shake.Core.Action.Internal
  ( Action(..)
  , runAfter
  ) where

import Control.Applicative
import Control.Monad.IO.Class

import B.Monad

newtype Action a = Action
  { toBuildRule :: BuildRule IO a }
  deriving (Functor, Applicative, Monad, MonadIO)

runAfter :: IO () -> Action ()
runAfter = error "Unimplemented: B.Shake.Action.Internal.<>"
