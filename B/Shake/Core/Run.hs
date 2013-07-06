{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B.Shake.Core.Run
  ( run
  ) where

import Data.Binary (Binary)
import Data.Semigroup
import Data.Typeable

import qualified Data.Binary as Binary

import B.Build
import B.Question
import B.Shake.Core.Action
import B.Shake.Core.Action.Internal (toBuildRule)
import B.Shake.Core.Rules
import B.Shake.Core.Rules.Internal (runRules)
import B.Shake.Types (ShakeOptions)
import B.Simple (evalBuildStringPath)

import qualified B.Rule as B
import qualified B.RuleDatabase as RuleDatabase

run :: ShakeOptions -> Rules () -> IO ()
run _ rules = do
  (ruleDatabase, actions) <- runRules rules
  let ruleDatabase' = RuleDatabase.insert FooR ruleDatabase
  evalBuildStringPath "build.db" ruleDatabase' (build (FooQ actions))

data FooQ = FooQ [Action ()]
  deriving (Typeable)

instance Eq FooQ where
  FooQ _ == FooQ _ = False

instance Show FooQ where
  show (FooQ _) = "FooQ"

instance Binary FooQ where
  get = do
    () <- Binary.get
    return $ FooQ []
  put (FooQ _) = Binary.put ()

instance Question FooQ where
  type Answer FooQ = ()
  type AnswerMonad FooQ = IO
  answer (FooQ _) = return $ Right ()

data FooR = FooR
  deriving (Typeable)

instance B.Rule FooQ FooR where
  queryRule (FooQ actions) FooR = [mapM_ toBuildRule actions]

instance Semigroup FooR where
  FooR <> FooR = FooR
