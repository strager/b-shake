{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module B.Shake.Core.Rules.Internal
  ( Rules

  , runRules
  , action
  , tellRule
  , rulesIO
  ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Data.Semigroup

import B.RuleDatabase
import B.Shake.Core.Action

newtype Rules a = Rules
  (WriterT (RuleDatabase IO)
    (WriterT [Action ()]
      IO) a)
  deriving (Functor, Applicative, Monad)

instance (Monoid a) => Monoid (Rules a) where
  mempty = error "Unimplemented: B.Shake.Rules.mempty"
  mappend = error "Unimplemented: B.Shake.Rules.mappend"

instance (Semigroup a) => Semigroup (Rules a) where
  (<>) = error "Unimplemented: B.Shake.Rules.<>"

runRules :: Rules () -> IO (RuleDatabase IO, [Action ()])
runRules (Rules m) = runWriterT $ execWriterT m

action :: Action a -> Rules ()
action act = Rules . lift $ tell [void act]

tellRule :: RuleDatabase IO -> Rules ()
tellRule = Rules . tell

rulesIO :: IO a -> Rules a
rulesIO = Rules . lift . lift
