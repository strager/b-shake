{-# LANGUAGE RecordWildCards #-}

module B.Shake.Locks(
    Lock, newLock, withLock,
    Var, newVar, readVar, modifyVar, modifyVar_, withVar,
    Barrier, newBarrier, signalBarrier, waitBarrier,
    Resource, newResourceIO, acquireResource, releaseResource
    ) where

import Control.Concurrent
import Control.Monad
import Data.Function
import System.IO.Unsafe


---------------------------------------------------------------------
-- LOCK

-- | Like an MVar, but has no value
newtype Lock = Lock (MVar ())
instance Show Lock where show _ = "Lock"

newLock :: IO Lock
newLock = fmap Lock $ newMVar ()

withLock :: Lock -> IO a -> IO a
withLock (Lock x) = withMVar x . const


---------------------------------------------------------------------
-- VAR

-- | Like an MVar, but must always be full
newtype Var a = Var (MVar a)
instance Show (Var a) where show _ = "Var"

newVar :: a -> IO (Var a)
newVar = fmap Var . newMVar

readVar :: Var a -> IO a
readVar (Var x) = readMVar x

modifyVar :: Var a -> (a -> IO (a, b)) -> IO b
modifyVar (Var x) f = modifyMVar x f

modifyVar_ :: Var a -> (a -> IO a) -> IO ()
modifyVar_ (Var x) f = modifyMVar_ x f

withVar :: Var a -> (a -> IO b) -> IO b
withVar (Var x) f = withMVar x f


---------------------------------------------------------------------
-- BARRIER

-- | Starts out empty, then is filled exactly once
newtype Barrier a = Barrier (MVar a)
instance Show (Barrier a) where show _ = "Barrier"

newBarrier :: IO (Barrier a)
newBarrier = fmap Barrier newEmptyMVar

signalBarrier :: Barrier a -> a -> IO ()
signalBarrier (Barrier x) = putMVar x

waitBarrier :: Barrier a -> IO a
waitBarrier (Barrier x) = readMVar x


---------------------------------------------------------------------
-- RESOURCE

{-# NOINLINE resourceIds #-}
resourceIds :: Var Int
resourceIds = unsafePerformIO $ newVar 0

resourceId :: IO Int
resourceId = modifyVar resourceIds $ \i -> let j = i + 1 in j `seq` return (j, j)


-- | A type representing a finite resource, which multiple build actions should respect.
--   Created with 'Development.Shake.newResource' and used with 'Development.Shake.withResource'
--   when defining rules.
--
--   As an example, only one set of calls to the Excel API can occur at one time, therefore
--   Excel is a finite resource of quantity 1. You can write:
--
-- @
-- 'Development.Shake.shake' 'Development.Shake.shakeOptions'{'Development.Shake.shakeThreads'=2} $ do
--    'Development.Shake.want' [\"a.xls\",\"b.xls\"]
--    excel <- 'Development.Shake.newResource' \"Excel\" 1
--    \"*.xls\" 'Development.Shake.*>' \\out ->
--        'Development.Shake.withResource' excel 1 $
--            'Development.Shake.system'' \"excel\" [out,...]
-- @
--
--   Now the two calls to @excel@ will not happen in parallel. Using 'Resource'
--   is better than 'MVar' as it will not block any other threads from executing. Be careful that the
--   actions run within 'Development.Shake.withResource' do not themselves require further quantities of this resource, or
--   you may get a \"thread blocked indefinitely in an MVar operation\" exception. Typically only
--   system commands (such as 'Development.Shake.system'') should be run inside 'Development.Shake.withResource',
--   not commands such as 'Development.Shake.need'. If an action requires multiple resources, use
--   'Development.Shake.withResources' to avoid deadlock.
--
--   As another example, calls to compilers are usually CPU bound but calls to linkers are usually
--   disk bound. Running 8 linkers will often cause an 8 CPU system to grid to a halt. We can limit
--   ourselves to 4 linkers with:
--
-- @
-- disk <- 'Development.Shake.newResource' \"Disk\" 4
-- 'Development.Shake.want' [show i 'Development.Shake.FilePath.<.>' \"exe\" | i <- [1..100]]
-- \"*.exe\" 'Development.Shake.*>' \\out ->
--     'Development.Shake.withResource' disk 1 $
--         'Development.Shake.system'' \"ld\" [\"-o\",out,...]
-- \"*.o\" 'Development.Shake.*>' \\out ->
--     'Development.Shake.system'' \"cl\" [\"-o\",out,...]
-- @
data Resource = Resource {resourceKey :: Int, resourceName :: String, resourceMax :: Int, resourceVar :: Var (Int,[(Int,IO ())])}
instance Show Resource where show Resource{..} = "Resource " ++ resourceName

instance Eq Resource where (==) = (==) `on` resourceKey
instance Ord Resource where compare = compare `on` resourceKey


-- | A version of 'newResource' that runs in IO, and can be called before calling 'Development.Shake.shake'.
--   Most people should use 'Development.Shake.newResource' instead.
newResourceIO :: String -> Int -> IO Resource
newResourceIO name mx = do
    when (mx < 0) $
        error $ "You cannot create a resource named " ++ name ++ " with a negative quantity, you used " ++ show mx
    key <- resourceId
    var <- newVar (mx, [])
    return $ Resource key name mx var


-- | Try to acquire a resource. Returns Nothing to indicate you have acquired with no blocking, or Just act to
--   say after act completes (which will block) then you will have the resource.
acquireResource :: Resource -> Int -> IO (Maybe (IO ()))
acquireResource r@Resource{..} want
    | want < 0 = error $ "You cannot acquire a negative quantity of " ++ show r ++ ", requested " ++ show want
    | want > resourceMax = error $ "You cannot acquire more than " ++ show resourceMax ++ " of " ++ show r ++ ", requested " ++ show want
    | otherwise = modifyVar resourceVar $ \(available,waiting) ->
        if want <= available then
            return ((available - want, waiting), Nothing)
        else do
            bar <- newBarrier
            return ((available, waiting ++ [(want,signalBarrier bar ())]), Just $ waitBarrier bar)


-- | You should only ever releaseResource that you obtained with acquireResource.
releaseResource :: Resource -> Int -> IO ()
releaseResource Resource{..} i = modifyVar_ resourceVar $ \(available,waiting) -> f (available+i) waiting
    where
        f i ((wi,wa):ws) | wi <= i = wa >> f (i-wi) ws
                         | otherwise = do (i,ws) <- f i ws; return (i,(wi,wa):ws)
        f i [] = return (i, [])
