{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 StandaloneDeriving,
 ScopedTypeVariables,
 FlexibleInstances,
 UndecidableInstances,
 MultiParamTypeClasses,
 FunctionalDependencies,
 OverlappingInstances,
 GADTs
 #-}
{- |
Module      :  Network.Remote.RPC.Internal.Runtime
Copyright   :  (c) Matthew Mirman 2012
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
Stability   :  experimental
Portability :  GeneralizedNewtypeDeriving, StandaloneDeriving, ScopedTypeVariables, FlexibleInstances, UndecidableInstances, MultiParamTypeClasses, FunctionalDependencies, IncoherentInstances, TypeFamilies

The functions for using a function as a service and calling a remote process
-}
module Network.Remote.RPC.Internal.Runtime ( WIO()
                                           , onHost
                                           , world
                                           , realRemoteCall
                                           , makeService
                                           , Host(setHost, getValue, getDataDefault)
                                           , Sendable()
                                           , Receivable()                                             
                                           , Servable()
                                           , Ref()
                                           , liftIO
                                           , runServer
                                           , runServerBG
                                           ) where

import Network.Remote.RPC.Internal.MultiServer (send, recv, AIO(), connectToService, addService, addServiceByName, ServiceID(..), startServer, Servable, track)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (ThreadId, forkOS)
import System.IO (Handle)
import Control.Concurrent.ForkableRPC
import Control.Exception.CatchableRPC


import System.IO.Unsafe
import Data.IORef

-- | @'Host' World@ declares that the world is a host.  It should
-- only have one constructor, and the location and port should be invariant
-- of the given constructor. 
-- Specifically, 'getLocation' and 'getPort' should work even if bottom is supplied.
class Host a where
  getDataDefault :: a -> (String, Integer)
  getValue :: a

  getData :: a -> IO (String, Integer)
  getData _ = do
    let (_ :: a, ioref) = topLevelSetter
    readIORef ioref
    
  -- | @'setHost' world hostname port@ resets the default values
  -- for the @world@ be carefull to only use this at the beginning of a program.
  -- note that this uses unsafe state to work. 
  setHost :: a -> String -> Integer -> IO ()
  setHost _ l p = do 
    let (_ :: a, ioref) = topLevelSetter
    writeIORef ioref (l,p)
  
  topLevelSetter :: (a, IORef (String,Integer))
  topLevelSetter = (getValue, unsafePerformIO $ do
                       newIORef $ getDataDefault (getValue :: a))
  
-- | @'WIO' w m a@ is a newtype over a server transformer that adds the phantom
-- host argument @w@
newtype WIO w m a = WIO { runWIO :: AIO m a }
                           
deriving instance Monad m => Monad (WIO w m)
deriving instance Functor m => Functor (WIO w m)
deriving instance MonadIO m => MonadIO (WIO w m)
deriving instance Forkable m => Forkable (WIO w m)
deriving instance Catchable m => Catchable (WIO w m)
instance Servable m => Servable (WIO w m)
instance MonadTrans (WIO w) where lift = WIO . lift

-- | 'runServer' runs a name server and doesn't return
runServer :: forall w m . (Servable m, Host w) => WIO w m () -> m ()
runServer m = do
  p <- liftIO $ snd <$> getData (getValue :: w)
  startServer p $ runWIO m

-- | 'runServerBG' runs a name server on a background thread and does return
runServerBG :: Host w => WIO w IO () -> IO ThreadId
runServerBG m = do
  tid <- forkOS $ runServer m
  return tid

world :: forall w m . (Servable m, Host w) => WIO w m w
world = return (getValue :: w)

-- | 'onHost' declares that the code is running on the given host.
-- it is usefull when a type inference is wanted, but the action
-- also needs to be made into a service and used as a remote procedure
onHost :: forall w m . (Servable m, Host w) => w -> WIO w m ()
onHost _ = return ()           

data Ref a a' = Ref String Integer ServiceID
              | Val String

deriving instance Show (Ref a a')
deriving instance Read (Ref a a')


class (Servable m, Servable m') => Receivable m a m' a' 
  | m a' -> a, m' a -> a', a -> m, a' -> m'  where
  getRefValue :: Host w' => w' -> Ref a a' -> AIO m' a'  
  
class (Host w, Servable m, Servable m') => Sendable w m a m' a' 
  | m a' w -> a , m' a -> a', a -> m w, a' -> m' where
  makeRefFrom :: w -> a -> AIO m (Ref a a')

instance (Show a, Read a', a ~ a', Servable m', Servable m, Host w) => Sendable w m a m' a' where
  makeRefFrom _ v = return $ Val $ show v
instance (Show a, Read a', a ~ a', Servable m', Servable m) => Receivable m a m' a' where  
  getRefValue _ (Val s) = return $ read s
  getRefValue _ _ = error "should not be a ref: in Runtime.hs - getRefValue"

instance (Sendable w m b m' b') => Sendable w m (WIO w m b) m' (WIO w' m' b') where
  makeRefFrom w act = do
    ptr <- addService $ \handle -> do
          bVal :: b <- runWIO act
          bRef :: Ref b b' <- makeRefFrom w bVal
          send handle bRef
    (l,p) <- liftIO $ getData w

    return $ Ref l p ptr
instance (Receivable m b m' b') => Receivable m (WIO w m b) m' (WIO w' m' b') where
  getRefValue w (Ref w' p s) = track (w',p,s) $ WIO $ do
    handle <- connectToService w' p s
    bRef :: Ref b b' <- recv handle
    getRefValue w bRef

instance (Receivable m' a' m a, Sendable w m b m' b') => Sendable w m (a -> b) m' (a' -> WIO w' m' b') where  
  makeRefFrom w f = do
    ptr <- addService $ \handle -> do
          aRef :: Ref a' a <- recv handle
          bVal <- f <$> getRefValue w aRef
          bRef :: Ref b b' <- makeRefFrom w bVal
          send handle bRef
    (l,p) <- liftIO $ getData w          
    return $ Ref l p ptr

instance (Sendable w' m' a' m a, Receivable m b m' b') => Receivable m (a -> b) m' (a' -> WIO w' m' b') where  
  getRefValue w (Ref w' p s) = track (w',p,s) $ \a -> WIO $ do
    aRef :: Ref a' a <- makeRefFrom (getValue :: w') a
    handle <- connectToService w' p s
    send handle aRef
    bRef :: Ref b b' <- recv handle
    getRefValue w bRef

fetchRefValue :: (Receivable m' a m a', Host w) => Ref a a' -> WIO w m a'
fetchRefValue ref = do
  w <- world
  WIO $ getRefValue w ref
  
newRef :: forall a a' w m m' . (Sendable w m a m' a', Host w) => a -> WIO w m (Ref a a')
newRef a = do
  w :: w <- world
  WIO $ makeRefFrom w a
  
sendVal :: forall a a' w m m' . (Sendable w m a m' a', Host w) => a' -> Handle -> a -> WIO w m ()
sendVal _ handle val = do
  r :: Ref a a' <- newRef val 
  send handle r

recvVal :: forall a a' w m m' . (Receivable m' a m a', Host w, Servable m) => a -> Handle -> WIO w m a'
recvVal _ handle = do
  t :: Ref a a' <- recv handle 
  r :: a' <- fetchRefValue t
  return r


class (Servable m, Host w) => RPC a a' m w | a' -> w, a' -> m  where
  realRemoteCallH :: a -> w -> String -> (Handle -> WIO w m ()) -> a'

instance ( Receivable m a m' a', Host w, Host w'
         , Servable m, Servable m') 
         => RPC (WIO w m a) (WIO w' m' a') m' w' where
  
  {-# INLINE realRemoteCallH #-}
  realRemoteCallH act _ nm putVals = do
    let w = getActWorld act
    (l,p) <- liftIO $ getData w
    handle <- connectToService l p $ LocName nm
    putVals handle
    recvVal (undefined :: a) handle

getActWorld :: forall w a m . Host w => WIO w m a -> w
getActWorld _ = getValue

instance (Sendable w' m a' m a, RPC b b' m w') => RPC (a -> b) (a' -> b') m w' where
  realRemoteCallH _ w nm putOldVals a = realRemoteCallH (undefined :: b) w nm putVal
    where putVal handle = do
            putOldVals handle
            sendVal (undefined :: a) handle a 


realRemoteCall :: forall a a' w m . RPC a a' m w => a -> String -> a'
realRemoteCall i n = realRemoteCallH i (getValue :: w) n $ const $ return ()

class (Host w, Servable m) => Service a m w | a -> w m where
  runOnService :: a -> Handle -> WIO w m ()

instance (Host w, Servable m, Sendable w m a m a') => Service (WIO w m a) m w where
  runOnService action handle = action >>= sendVal (undefined :: a') handle

instance (Receivable m a' m a, Service b m w) => Service (a -> b) m w where
  runOnService foo handle = do
    val <- recvVal (undefined :: a') handle 
    runOnService (foo val) handle

makeService :: Service a m w => a -> String -> WIO w m ()
makeService fun nm = do
  WIO $ addServiceByName nm $ runWIO . runOnService fun
  return ()
