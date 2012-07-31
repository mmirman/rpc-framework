{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 StandaloneDeriving,
 ScopedTypeVariables,
 FlexibleInstances,
 UndecidableInstances,
 MultiParamTypeClasses,
 FunctionalDependencies,
 IncoherentInstances,
 TypeFamilies
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
                                           , realRemoteCall
                                           , makeService
                                           , Host(..)
                                           , Sendable()
                                           , Servable()
                                           , Ref()
                                           , liftIO
                                           , runServer
                                           , runServerBG
                                           ) where

import Network.Remote.RPC.Internal.MultiServer (send, recv, AIO(), connectToService, addService, addServiceByName, ServiceID(..), startServer, Servable)
import Control.Monad.Trans.Class (MonadTrans(..))
import Data.Functor ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (ThreadId, forkOS)
import System.IO (Handle)
import Control.Concurrent.Forkable

-- | @'Host' World@ declares that the world is a host.  It should
-- only have one constructor, and the location and port should be invariant
-- of the given constructor. 
-- Specifically, 'getLocation' and 'getPort' should work even if bottom is supplied.
class Host a where
  getLocation :: a -> String
  getPort :: a -> Integer
  getValue :: a
  
-- | @'WIO' w m a@ is a newtype over a server transformer that adds the phantom
-- host argument @w@
newtype WIO w m a = WIO { runWIO :: AIO m a }
                           
deriving instance Monad m => Monad (WIO w m)
deriving instance Functor m => Functor (WIO w m)
deriving instance MonadIO m => MonadIO (WIO w m)
deriving instance Forkable m => Forkable (WIO w m)
instance Servable m => Servable (WIO w m)
instance MonadTrans (WIO w) where lift = WIO . lift

-- | 'runServer' runs a name server and doesn't return
runServer :: forall w m . (Servable m, Host w) => WIO w m () -> m ()
runServer = startServer (getPort (undefined :: w)) . runWIO 

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
               
class (Servable m) => Sendable m a a' | a' -> a, a m -> a', a a' -> m where
  getRefValue :: Host w => w -> Ref a a' -> AIO m a'  
  makeRefFrom :: Host w => w -> a -> AIO m (Ref a a')
  
instance (Read a, Show a', a ~ a',  Servable m) => Sendable m a a' where
  makeRefFrom _ v = return $ Val (show v)
  getRefValue _ (Val s) = return $ read s
  getRefValue _ _ = error "should not be a ref: in Runtime.hs - getRefValue"
  
instance (Sendable m a' a, Sendable m b b') => Sendable m (a -> b) (a' -> WIO w m b') where  
  makeRefFrom w f = do
    ptr <- addService $ \handle -> do
          aRef :: Ref a' a <- recv handle
          bVal <- f <$> getRefValue w aRef
          bRef :: Ref b b' <- makeRefFrom w bVal
          send handle bRef
    return $ Ref (getLocation w) (getPort w) ptr

  {-# NOINLINE getRefValue #-}
  getRefValue _ (Val _) = error "should not be a value: in Runtime.hs - getRefValue"
  getRefValue w (Ref w' p s) = do
    return $ \a -> WIO $ do
      aRef :: Ref a' a <- makeRefFrom w a
      handle <- connectToService w' p s
      send handle aRef
      bRef :: Ref b b' <- recv handle
      getRefValue w bRef

fetchRefValue :: (Sendable m a a', Host w, Servable m) => Ref a a' -> WIO w m a'
fetchRefValue ref = do
  w <- world
  WIO $ getRefValue w ref
  
newRef :: forall a a' w m . (Sendable m a a', Host w, Servable m) => a -> WIO w m (Ref a a')
newRef a = do
  w :: w <- world
  WIO $ makeRefFrom w a
  
sendVal :: forall a a' w m . (Sendable m a a', Host w, Servable m) => a' -> Handle -> a -> WIO w m ()
sendVal _ handle val = do
  r :: Ref a a' <- newRef val 
  send handle r

recvVal :: forall a a' w m . (Sendable m a a', Host w, Servable m) => a -> Handle -> WIO w m a'
recvVal _ handle = do
  t :: Ref a a' <- recv handle 
  r :: a' <- fetchRefValue t
  return r


class (Servable m, Host w) => RPC a a' m w | a' -> w, a' -> m  where
  realRemoteCallH :: a -> w -> String -> (Handle -> WIO w m ()) -> a'

instance ( Sendable m' a a', Host w, Host w'
         , Servable m, Servable m') 
         => RPC (WIO w m a) (WIO w' m' a') m' w' where
  
  {-# INLINE realRemoteCallH #-}
  realRemoteCallH act _ nm putVals = do
    let w = getActWorld act
    handle <- connectToService (getLocation w) (getPort w) $ LocName nm
    putVals handle
    recvVal (undefined :: a) handle

getActWorld :: forall w a m . Host w => WIO w m a -> w
getActWorld _ = getValue

instance (Sendable m a' a, RPC b b' m w') => RPC (a -> b) (a' -> b') m w' where
  {-# INLINE realRemoteCallH #-}
  realRemoteCallH _ w nm putOldVals a = realRemoteCallH (undefined :: b) w nm putVal
    where putVal handle = do
            putOldVals handle
            sendVal (undefined :: a) handle a 

{-# INLINE realRemoteCall #-}
realRemoteCall :: forall a a' w m . RPC a a' m w => a -> String -> a'
realRemoteCall i n = realRemoteCallH i (getValue :: w) n $ const $ return ()

class (Host w, Servable m) => Service a m w | a -> w m where
  runOnService :: a -> Handle -> WIO w m ()

instance (Host w, Servable m, Sendable m a a') => Service (WIO w m a) m w where
  runOnService action handle = action >>= sendVal (undefined :: a') handle

instance (Sendable m a' a, Service b m w) => Service (a -> b) m w where
  runOnService foo handle = do
    val <- recvVal (undefined :: a') handle 
    runOnService (foo val) handle

makeService :: Service a m w => a -> String -> WIO w m ()
makeService fun nm = do
  WIO $ addServiceByName nm $ runWIO . runOnService fun
  return ()