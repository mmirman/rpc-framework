{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 FlexibleInstances,
 MultiParamTypeClasses,
 StandaloneDeriving,
 ScopedTypeVariables
 #-}
{- |
Module      :  Network.Remote.RPC.Internal.MultiServer
Copyright   :  (c) Matthew Mirman 2012
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
Stability   :  experimental
Portability :   GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses, StandaloneDeriving, ScopedTypeVariables

A server that allows for the declaration of services and for the 
calling of services.
-}
module Network.Remote.RPC.Internal.MultiServer where
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (MonadReader(..), runReaderT, ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forever)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, withMVar, MVar())
import System.IO (Handle, hWaitForInput, hFlush, hGetLine, hClose, hPutStrLn)
import Network (connectTo, accept, PortID(PortNumber), listenOn)
import Data.Functor ((<$>))
import Control.Concurrent.Forkable
import System.Mem.Weak

data ServiceID = LocNumber Integer
               | LocName String
               deriving (Show, Read, Ord, Eq)

data Message = Talk ServiceID
             | Kill ServiceID
             deriving (Show, Read, Ord, Eq)

type Handlers m = M.Map ServiceID (Handle -> AIO m ())

type State m = MVar (Handlers m, Integer)
newtype AIO m a = AIO (ReaderT (State m) m a)

deriving instance Monad m => Monad (AIO m)
deriving instance Functor m => Functor (AIO m)
deriving instance MonadIO m => MonadIO (AIO m)
deriving instance Forkable m => Forkable (AIO m)
instance MonadTrans AIO where lift = AIO . lift

instance Monad m => MonadReader (State m) (AIO m) where
  ask = AIO ask
  local f (AIO m) = AIO (local f m)
  
send handle val = liftIO $ do
  hPutStrLn handle (show val) 
  hFlush handle

recv handle = liftIO $ do 
  hWaitForInput handle $ -1 
  read <$> hGetLine handle

startServer :: forall m a . (Servable m) => Integer -> AIO m a -> m a
startServer port (AIO aio) = do
  r <- liftIO $ newMVar (mempty :: Handlers m, 0)
  forkIO $ do
    s <- liftIO $ listenOn $ PortNumber $ fromInteger port
    forever $ do
      (handle, _, _) <- liftIO $ accept s
      message <- recv handle
      
      case message of
        Kill service -> do
          liftIO $ modifyMVar_ r $ \(map,t) -> return $ (M.delete service map,t)
        Talk service -> do
          f <- liftIO $ withMVar r $ \(map,_) -> return $ safeFind map service
          forkIO $ case f handle of
            AIO m -> do
              runReaderT m r
              liftIO $ hClose handle
          return ()    
  runReaderT aio r
  
safeFind map val = case M.lookup val map of
  Just r -> r
  Nothing -> error $ "can't find "++ show val++" in server map: "++show (M.keys map)

--unsafePerformServer :: MonadIO m => State -> AIO m a -> a
--unsafePerformServer handlers (AIO aio) = unsafePerformIO $ liftIO $ runReaderT aio handlers

getHandlers :: Monad m => AIO m (State m)
getHandlers = ask

addService :: (Monad m, MonadIO m) => (Handle -> AIO m ()) -> AIO m ServiceID
addService handler = do
  r <- getHandlers
  liftIO $ modifyMVar r $ \(map, i) -> do 
    let i' = i+1
    return ((M.insert (LocNumber i') handler map , i'), LocNumber i')

addServiceByName :: (Monad m, MonadIO m) => String -> (Handle -> AIO m ()) -> AIO m ServiceID
addServiceByName nm handler = do
  r <- getHandlers
  liftIO $ modifyMVar r $ \(map, i) -> do 
    let t = LocName nm
    return ((M.insert t handler map , i), t)

connectToService :: MonadIO m => String -> Integer -> ServiceID -> m Handle
connectToService host port service = do
  handle <- liftIO $ connectTo host $ PortNumber $ fromInteger port
  send handle $ Talk service
  return handle

-- | 'Servable' is a declaration that the given monad can be made into a 
-- servlet.
class (Functor m, Monad m, MonadIO m, Forkable m) => Servable m 
instance Servable IO
instance Servable m => Servable (AIO m)


track :: Servable m => (String, Integer, ServiceID) -> a -> AIO m a
track (host,port,service) val = do
  wv <- liftIO $ mkWeakPtr val $ Just $ do
    handle <- connectTo host $ PortNumber $ fromInteger port
    send handle $ Kill service
  return val