module Control.Exception.CatchableRPC where

import qualified Control.Exception as B
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.State (StateT, runStateT, get, put)


class Monad m => Catchable m where
  bracket :: m a -> (a -> m b) -> (a -> m c) -> m c
  
instance Catchable IO where
  bracket = B.bracket
  
instance Catchable m => Catchable (ReaderT r m) where  
  bracket m mf1 mf2 = do
    env <- ask
    lift $ bracket (runReaderT m env) (\s -> runReaderT (mf1 s) env) (\s -> runReaderT (mf2 s) env)

instance (Catchable m) => Catchable (StateT s m) where    
  bracket m mf1 mf2 = do
    st <- get
    (a,st) <- lift $ bracket (runStateT m st)
              (\(a,state) -> runStateT (mf1 a) state)
              (\(a,state) -> runStateT (mf2 a) state)
    put st 
    return a

  