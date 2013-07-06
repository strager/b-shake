{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B.Shake.Core.Rules
  ( Rules

  , action
  , defaultRule
  , rule
  , withoutActions
  ) where

import Data.IntMap (IntMap)
import Data.Maybe (mapMaybe)
import Data.Semigroup
import Data.Typeable (Typeable)

import qualified Data.IntMap as IntMap
import qualified Data.List.NonEmpty as NonEmpty

import B.Shake.Core.Action
import B.Shake.Core.Rule.Internal (Rule, RuleExecutor(..))
import B.Shake.Core.Rules.Internal (Rules, action, tellRule)

import qualified B.Question as B
import qualified B.Rule as B
import qualified B.RuleDatabase as RuleDatabase

newtype PrioritizedRules q r = PrioritizedRules (IntMap r)
  deriving (Typeable)

instance (Semigroup r) => Semigroup (PrioritizedRules q r) where
  PrioritizedRules xs <> PrioritizedRules ys
    = PrioritizedRules $ IntMap.unionWith (<>) xs ys

instance (B.Question q, B.Rule q r, Typeable r)
  => B.Rule q (PrioritizedRules q r) where
  queryRule q (PrioritizedRules rules)
    = case priorityRules of
      [] -> []
      (subrules:_) -> NonEmpty.toList subrules
    where
    priorityRules
      = mapMaybe (NonEmpty.nonEmpty . B.traceQueryRule q . snd)
      $ IntMap.toDescList rules

defaultRule, rule
  :: (Rule key value)
  => (key -> Maybe (Action value))
  -> Rules ()
defaultRule = rulePriority 0
rule = rulePriority 1

rulePriority
  :: (Rule key value)
  => Int
  -> (key -> Maybe (Action value))
  -> Rules ()
rulePriority priority
  = tellRule . RuleDatabase.singleton
  . PrioritizedRules . IntMap.singleton priority
  . B.singletonRules . RuleExecutor

withoutActions :: Rules () -> Rules ()
withoutActions = error "Unimplemented: B.Shake.Core.Rules.withoutActions"
