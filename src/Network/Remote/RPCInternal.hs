{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 StandaloneDeriving,
 ScopedTypeVariables,
 FlexibleInstances,
 UndecidableInstances,
 TypeFamilies, 
 MultiParamTypeClasses,
 IncoherentInstances,
 FunctionalDependencies
 #-}
module Network.Remote.RPCInternal ( WIO()
                                  , world
                                  , realRemoteCall
                                  , remoteCall
                                  , makeService
                                  , Host(..)
                                  , Sendable()
                                  , Ref()
                                  , liftIO
                                  , runServer
                                  , runServerBG
                                  ) where


import Network.Remote.MultiServer
import Data.Functor ((<$>))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent (forkIO, ThreadId, forkOS)
import System.IO (Handle)

class Host a where
  getLocation :: a -> String
  getPort :: a -> Integer
  getValue :: a
  
newtype WIO w a = WIO { runWIO :: AIO a }
  
liftAIO = WIO
                  
deriving instance Monad (WIO w)
deriving instance Functor (WIO w)
deriving instance MonadIO (WIO w)

-- | 'runServer' runs a name server and doesn't return
runServer :: forall w . Host w => WIO w () -> IO ()
runServer = startServer (getPort (undefined :: w)) . runWIO 

-- | 'runServerBG' runs a name server on a background thread and does return
runServerBG :: Host w => WIO w () -> IO ThreadId
runServerBG m = do
  tid <- forkOS $ runServer m
  return tid

world :: forall w . Host w => WIO w w
world = return (getValue :: w)

data Ref a = Ref String Integer ServiceID
           | Val String
           deriving (Show, Read)

class Sendable a where
  getRefValue :: Host w => w -> Ref a -> AIO a
  makeRefFrom :: Host w => w -> a -> AIO (Ref a)
    
instance (Read a, Show a) => Sendable a where
  makeRefFrom _ v = return $ Val (show v)
  getRefValue _ (Val s) = return $ read s

instance (Sendable a, Sendable b) => Sendable (a -> b) where
  makeRefFrom w f = do
    ptr <- addService $ \handle -> do
          aRef <- recv handle
          bVal <- f <$> getRefValue w aRef
          bRef <- makeRefFrom w bVal
          send handle bRef
    return $ Ref (getLocation w) (getPort w) ptr

  {-# NOINLINE getRefValue #-}
  getRefValue w (Ref w' p s) = do
    state <- getHandlers 
    return $ \a -> unsafePerformServer state $ do
      aRef <- makeRefFrom w a
      handle <- connectToService w' p s
      send handle aRef
      bRef <- recv handle
      getRefValue w bRef

fetchRefValue :: (Sendable a , Host w) => Ref a -> WIO w a
fetchRefValue ref = do
  w <- world
  liftAIO $ getRefValue w ref 
  
newRef :: (Sendable a , Host w) => a -> WIO w (Ref a)
newRef a = do
  w <- world
  liftAIO $ makeRefFrom w a

sendVal :: (Sendable a, Host w) => Handle -> a -> WIO w ()    
sendVal handle val = newRef val >>= send handle

recvVal :: (Sendable a, Host w) => Handle -> WIO w a
recvVal handle = recv handle >>= fetchRefValue

class Host w => RPC a w where
  type Initialize a w
  
  remoteCall :: a -> Initialize a w
  remoteCall = undefined
  
  realRemoteCallH :: a -> w -> String -> (Handle -> WIO w ()) -> Initialize a w

instance (Sendable a, Host w, Host w') => RPC (WIO w a) w' where
  
  type Initialize (WIO w a) w' = WIO w' a
  
  {-# INLINE realRemoteCallH #-}
  realRemoteCallH act _ nm putVals = do
    let host = getLocation (getValue :: w)
    let w = getActWorld act
    handle <- connectToService (getLocation w) (getPort w) $ LocName nm
    putVals handle
    recvVal handle

getActWorld :: forall w a . Host w => WIO w a -> w
getActWorld _ = getValue

instance (Sendable a, RPC b w') => RPC (a -> b) w' where
  type Initialize (a -> b) w' = a -> Initialize b w'
  
  {-# INLINE realRemoteCallH #-}
  realRemoteCallH _ w nm putOldVals a = realRemoteCallH (undefined :: b) w nm putVal
    where putVal handle = do
            putOldVals handle
            sendVal handle a
  
{-# INLINE realRemoteCall #-}
realRemoteCall :: forall a w . (Host w , RPC a w) => a -> String -> Initialize a w
realRemoteCall i n = realRemoteCallH i (getValue :: w) n $ const $ return ()

makeService :: Service a w => a -> String -> WIO w ()
makeService fun nm = do
  liftAIO $ addServiceByName nm $ runWIO . runOnService fun
  return ()
  
class Host w => Service a w | a -> w where
  runOnService :: a -> Handle -> WIO w ()
  
instance (Host w, Sendable a) => Service (WIO w a) w where
  runOnService action handle = action >>= sendVal handle
  
instance (Sendable a, Service b w) => Service (a -> b) w where
  runOnService foo handle = do
    val <- recvVal handle 
    runOnService (foo val) handle