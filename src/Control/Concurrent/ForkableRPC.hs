module Control.Concurrent.ForkableRPC where

import Control.Concurrent hiding (forkIO)
import qualified Control.Concurrent as F
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.State (StateT, runStateT, get)

class Monad m => Forkable m where
  forkIO :: m a -> m ThreadId
  
instance Forkable IO where  
  forkIO m = F.forkIO (m >> return ())
  
instance Forkable m => Forkable (ReaderT r m) where  
  forkIO m = do
    env <- ask
    lift $ forkIO $ runReaderT m env

instance (Forkable m) => Forkable (StateT s m) where    
  forkIO act = do
    st <- get
    lift $ forkIO (runStateT act st)