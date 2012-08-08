{-# LANGUAGE TemplateHaskell, KindSignatures, FlexibleContexts #-}
module Main where

import Network.Remote.RPC

putText :: String -> WIO w IO ()
putText str = lift $ putStrLn str

-- define worlds and their defaults.
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

  talk <- $(rpcCall 'delayTalkServer)
  talk
    
doubleServer :: Integer -> WIO Server IO Integer
doubleServer t = return $ t + t

addServer :: Integer -> WIO Server IO (Integer -> Integer)
addServer t = do
  return (t +)

delayTalkServer = do
  onHost Server
  putText "I am called now "
  return $ do
    putText "I am called later "

vlad :: WIO Client IO (WIO Client IO ())
vlad = $(rpcCall 'delayTalkServer)

talkServer given = do
  onHost Server
  putText $ "On Server: "++given

main = do
  -- reset the defaults.  we can set these from a config file if necessary
  setHost Server "localhost" 9006
  setHost Client "localhost" 9004
    
  runServerBG $(autoService 'Server)
  -- one of these has to not return, otherwise the program will exit
  runServer client

