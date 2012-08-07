{-# LANGUAGE TemplateHaskell, KindSignatures, FlexibleContexts #-}
module Main where

import Network.Remote.RPC
putText str = lift $ putStrLn str

$(makeHost "Client" "localhost" 9000)
$(makeHost "Server" "localhost" 9001)
  
client :: WIO Client IO ()
client = do
  onHost Client
  
  double <- $(rpcCall 'doubleServer) 3
  putText $ "r(h3+h3) ? " ++ show double
         
  add <- $(rpcCall 'addServer) 4
  t <- add 8
  putText $ "r(h4 +) h8 ? " ++ show t
  r <- add 6
  putText $ "r(h4 +) h6 ? " ++ show r
  
  $(rpcCall 'talkServer) "hi"
    
doubleServer :: Integer -> WIO Server IO Integer
doubleServer t = return $ t + t

addServer :: Integer -> WIO Server IO (Integer -> Integer)
addServer t = do
  return (t +)

talkServer given = do
  onHost Server
  putText $ "On Server: "++given
  
main = do
  runServerBG $(autoService 'Server)
--  runServerBG $(makeServices [ 'addServer , 'doubleServer])
  -- one of these has to not return, otherwise the program will exit
  runServer client

