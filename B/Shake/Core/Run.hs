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
  let ruleDatabase' = RuleDatabase.insert RootR ruleDatabase
  evalBuildStringPath "build.db" ruleDatabase' (build (RootQ actions))

data RootQ = RootQ [Action ()]
  deriving (Typeable)

instance Eq RootQ where
  RootQ _ == RootQ _ = False

instance Show RootQ where
  show (RootQ _) = "RootQ"

instance Binary RootQ where
  get = do
    () <- Binary.get
    return $ RootQ []
  put (RootQ _) = Binary.put ()

instance Question RootQ where
  type Answer RootQ = ()
  type AnswerMonad RootQ = IO
  answer (RootQ _) = return $ Right ()

data RootR = RootR
  deriving (Typeable)

instance B.Rule RootQ RootR where
  queryRule (RootQ actions) RootR = [mapM_ toBuildRule actions]

instance Semigroup RootR where
  RootR <> RootR = RootR
