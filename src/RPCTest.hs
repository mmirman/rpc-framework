{-# LANGUAGE TemplateHaskell, KindSignatures, FlexibleContexts #-}
module Main where

import Network.Remote.RPC

putText str = liftIO $ putStrLn str

$(makeHost "Client" "localhost" 9000)
$(makeHost "Server" "localhost" 9001)
  
client :: WIO Client IO ()
client = do
  Client <- world
  
  double <- $(rpcCall 'doubleServer) 3
  putText $ "r(h3+h3) ? " ++ show double
         
  add <- $(rpcCall 'addServer) 4
  r <- add 6
  
  putText $ "r(h4 +) h6 ? " ++ show r

doubleServer :: Integer -> WIO Server IO Integer
doubleServer t = do
  Server <- world
  return (t + t :: Integer)

addServer :: Integer -> WIO Server IO (Integer -> Integer)
addServer t = do
  Server <- world
  return (t +)

main = do
  runServerBG $(autoService 'Server)
--  runServerBG $(makeServices [ 'addServer , 'doubleServer])
  -- one of these has to not return, otherwise the program will exit
  runServer client