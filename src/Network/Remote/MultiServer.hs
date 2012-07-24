{-# LANGUAGE 
 GeneralizedNewtypeDeriving,
 FlexibleInstances,
 MultiParamTypeClasses
 #-}
module Network.Remote.MultiServer where

import Control.Monad.Reader (MonadReader(..), runReaderT, ReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (forever)
import qualified Data.Map as M
import Data.Monoid (mempty)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (modifyMVar, newMVar, withMVar, MVar())
import System.IO (Handle, hWaitForInput, hFlush, hGetLine, hClose, hPutStrLn)
import Network (connectTo, accept, PortID(PortNumber), listenOn)
import Data.Functor ((<$>))
import System.IO.Unsafe (unsafePerformIO)

data ServiceID = LocNumber Integer
               | LocName String
               deriving (Show, Read, Ord, Eq)

type Handlers = M.Map ServiceID (Handle -> AIO ())
type State = MVar (Handlers, Integer)
newtype AIO a = AIO (ReaderT State IO a)
              deriving (Monad, Functor, MonadIO)



instance MonadReader State AIO where
  ask = AIO ask
  local f (AIO m) = AIO (local f m)
  
send handle val = liftIO $ do
  hPutStrLn handle (show val) 
  hFlush handle

recv handle = liftIO $ do 
  hWaitForInput handle $ -1 
  read <$> hGetLine handle

startServer :: Integer -> AIO a -> IO a
startServer port (AIO aio) = do
  r <- newMVar (mempty :: Handlers, 0)
  forkIO $ do
    s <- listenOn $ PortNumber $ fromInteger port
    forever $ do
      (handle, host, port) <- accept s
      service <- recv handle
      f <- withMVar r $ \(map,_) -> return $ safeFind map service
      forkIO $ case f handle of
        AIO m -> do
          runReaderT m r
          hClose handle
  runReaderT aio r
  
safeFind map val = case M.lookup val map of
  Just r -> r
  Nothing -> error $ "can't find "++ show val++" in server map: "++show (M.keys map)

unsafePerformServer :: State -> AIO a -> a
unsafePerformServer handlers (AIO aio) = unsafePerformIO $ runReaderT aio handlers

getHandlers :: AIO State
getHandlers = ask

addService :: (Handle -> AIO ()) -> AIO ServiceID
addService handler = do
  r <- getHandlers
  liftIO $ modifyMVar r $ \(map, i) -> do 
    let i' = i+1
    return ((M.insert (LocNumber i') handler map , i'), LocNumber i')

addServiceByName :: String -> (Handle -> AIO ()) -> AIO ServiceID
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
