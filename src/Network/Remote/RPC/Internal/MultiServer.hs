{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 FlexibleInstances,
 MultiParamTypeClasses,
 StandaloneDeriving,
 ScopedTypeVariables
 #-}
module Network.Remote.RPC.Internal.MultiServer where
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Reader (MonadReader(..), runReaderT, ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forever)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Control.Concurrent.MVar (modifyMVar, newMVar, withMVar, MVar())
import System.IO (Handle, hWaitForInput, hFlush, hGetLine, hClose, hPutStrLn)
import Network (connectTo, accept, PortID(PortNumber), listenOn)
import Data.Functor ((<$>))
import Control.Concurrent.Forkable

data ServiceID = LocNumber Integer
               | LocName String
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

startServer :: forall m a . (Forkable m, Monad m, MonadIO m) => Integer -> AIO m a -> m a
startServer port (AIO aio) = do
  r <- liftIO $ newMVar (mempty :: Handlers m, 0)
  forkIO $ do
    s <- liftIO $ listenOn $ PortNumber $ fromInteger port
    forever $ do
      (handle, _, _) <- liftIO $ accept s
      service <- recv handle
      f <- liftIO $ withMVar r $ \(map,_) -> return $ safeFind map service
      forkIO $ case f handle of
        AIO m -> do
          runReaderT m r
          liftIO $ hClose handle
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
  send handle service
  return handle


class (Functor m, Monad m, MonadIO m, Forkable m) => Servable m 
instance Servable IO
instance Servable m => Servable (AIO m)

