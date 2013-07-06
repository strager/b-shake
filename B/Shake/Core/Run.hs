{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module B.Shake.Core.Run
  ( run
  ) where

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Data.Binary (Binary)
import Data.Semigroup
import Data.Typeable
import Data.Typeable.Internal (TypeRep(TypeRep))
import System.Directory

import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import qualified Data.ByteString.Lazy as BSLazy

import B.Build
import B.Monad
import B.Oracle.Binary
import B.Question
import B.RuleDatabase (RuleDatabase)
import B.Shake.Core.Action
import B.Shake.Core.Action.Internal (toBuildRule)
import B.Shake.Core.Rules
import B.Shake.Core.Rules.Internal (runRules)
import B.Shake.Types (ShakeOptions)

import qualified B.Oracle as Oracle
import qualified B.Oracle.InMemory as InMemory
import qualified B.Oracle.InMemory.Pure as OraclePure
import qualified B.Rule as B
import qualified B.RuleDatabase as RuleDatabase

run :: ShakeOptions -> Rules () -> IO ()
run _ rules = do
  (ruleDatabase, actions) <- runRules rules
  let ruleDatabase' = RuleDatabase.insert FooR ruleDatabase
  go "build.db" ruleDatabase' (build (FooQ actions))

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

go
  :: FilePath
  -> RuleDatabase IO
  -> Build IO a
  -> IO a
go dbPath ruleDatabase m = do
  exists <- doesFileExist dbPath
  state <- if exists
    then readOracle dbPath
    else return OraclePure.empty

  storage <- newMVar state
  let oracle = InMemory.mkMVarOracleWithStorage storage
  Oracle.recheckAll oracle

  mResult <- runBuild ruleDatabase oracle logMessage m

  state' <- takeMVar storage
  writeOracle dbPath state'

  either (throwIO . head) return mResult

  where
  logMessage :: (Show a) => a -> IO ()
  logMessage x = putStrLn ("> " ++ show x)

  lookupQuestion
    :: Fingerprint
    -> Maybe (AQuestion IO{-undefined-})
  lookupQuestion fingerprint = RuleDatabase.lookupQuestion
    (TypeRep fingerprint undefined undefined)
    ruleDatabase

  readOracle
    :: FilePath
    -> IO (OraclePure.State IO)
  readOracle path = decode `fmap` BSLazy.readFile path
    where
    decode = Binary.runGet (OraclePure.getState lookupQuestion)

  writeOracle :: FilePath -> OraclePure.State IO -> IO ()
  writeOracle path
    = BSLazy.writeFile path
    . Binary.runPut
    . OraclePure.putState
