module B.Shake.Core.Action
  ( Action

  , apply
  , apply1
  , actionOnException
  , actionFinally
  ) where

import B.Shake.Core.Action.Internal (Action(..))
import B.Shake.Core.Rule.Internal (Rule, RuleKey(..))

import qualified B.Build as B

apply :: (Rule key value) => [key] -> Action [value]
apply keys = Action $ mapM B.need qs
  where
  qs = map RuleKey keys

apply1 :: (Rule key value) => key -> Action value
apply1 key = Action $ B.need (RuleKey key)

actionOnException :: Action a -> IO b -> Action a
actionOnException = error "Unimplemented: B.Shake.Action.actionOnException"

actionFinally :: Action a -> IO b -> Action a
actionFinally = error "Unimplemented: B.Shake.Action.actionFinally"
