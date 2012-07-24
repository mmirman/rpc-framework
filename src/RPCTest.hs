{-# LANGUAGE 
 TemplateHaskell, 
 ScopedTypeVariables
 #-}
module Main where

import Network.Remote.RPC

putText str = liftIO $ putStrLn str

$(makeHost "Client" "localhost" 9000)
$(makeHost "Server" "localhost" 9001)
  
client = do
  Client <- world
  
  double <- $(rpcCall 'doubleServer) 3
  putText $ "r(h3+h3) ? " ++ show double
         
  add <- $(rpcCall 'addServer) 4
  putText $ "r(h4 +) h6 ? " ++ show (add 6)         

doubleServer (t :: Integer) = do
  Server <- world
  return (t + t)

addServer (t :: Integer) = do
  Server <- world
  return (t +)

main = do
  runServerBG $(makeServices [ 'addServer , 'doubleServer])
  -- one of these has to not return, otherwise the program will exit
  runServer client    