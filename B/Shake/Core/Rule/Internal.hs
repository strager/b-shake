{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}  -- FIXME

module B.Shake.Core.Rule.Internal
  ( ShakeValue
  , Rule(..)

  , RuleKey(..)
  , RuleExecutor(..)
  ) where

import Control.Applicative
import Control.Monad (join, void)
import Data.Maybe (maybeToList)

import qualified Control.Exception as Ex

import B.Question
import B.Shake.Classes
import B.Shake.Core.Action.Internal (Action(..))

import qualified B.Rule as B

type ShakeValue a = (Show a, Typeable a, Eq a, Hashable a, Binary a, NFData a)

class (ShakeValue key, ShakeValue value) => Rule key value where
  storedValue :: key -> IO (Maybe value)

-- | What Shake calls 'Rule' we call 'Question'.  'RuleKey'
-- wraps a Shake 'Rule's key so it can be an instance of
-- 'Question'.
newtype RuleKey key value = RuleKey key
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

data NoValue = NoValue
  deriving (Show, Typeable)

instance Ex.Exception NoValue

instance (Rule key value) => Question (RuleKey key value) where
  type Answer (RuleKey key value) = value
  type AnswerMonad (RuleKey key value) = IO
  answer (RuleKey key) = fmap join . Ex.try $ do
    mValue <- storedValue key
    return $ maybe (Left (Ex.toException NoValue)) Right mValue

-- | Shake does not have a class or type corresponding to
-- b's 'Rule'.  Instead, it uses 'key -> Maybe (Action
-- value)' inline.
newtype RuleExecutor key value
  = RuleExecutor (key -> Maybe (Action value))
  deriving (Typeable)

instance (Rule key value)
  => B.Rule (RuleKey key value) (RuleExecutor key value) where
  queryRule (RuleKey key) (RuleExecutor f)
    = maybeToList $ (void . toBuildRule) <$> f key
