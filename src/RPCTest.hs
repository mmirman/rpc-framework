{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Main where

import Network.Remote.RPC

putText :: String -> WIO w IO ()
putText str = lift $ putStrLn str

-- define worlds and their defaults.
$(makeHost "Client" "localhost" 9000)
$(makeHost "ServerA" "localhost" 9001)
$(makeHost "ServerB" "localhost" 9002)
  
client :: WIO Client IO ()
client = do
  add <- $(rpcCall 'addServer) 4
  t <- add 8
  putText $ "r(h4 +) h8 ? " ++ show t

  talk1 <- $(rpcCall 'recursiveServerB) 4
  talk2 <- $(rpcCall 'recursiveServerA) 4
  t <- talk1
  putText $ " On Client: " ++ t             
  t <- talk2
  putText $ " On Client: " ++ t

addServer :: Integer -> WIO ServerA IO (Integer -> Integer)
addServer t = do
  return (t +)
  
recursiveServerA :: Int -> WIO ServerA IO (WIO ServerA IO String)
recursiveServerA 0 = return $ return "goose"
recursiveServerA (i :: Int) = do
  onHost ServerA
  putText "I am A"
  return $ do
    ret <- $(rpcCall 'recursiveServerB) (i-1)
    putText "I am again A "
    ret
    

recursiveServerB :: Int -> WIO ServerB IO (WIO ServerB IO String)
recursiveServerB 0 = return $ return "duck"
recursiveServerB i = do
  onHost ServerB
  putText "I am B"
  return $ do
    ret <- $(rpcCall 'recursiveServerA) (i-1)
    putText "I am again B"
    ret

main = do
  -- reset the defaults.  we can set these from a config file if necessary
  setHost Client "localhost" 9004
  

  runServerBG $(autoService 'ServerA)
  runServerBG $(autoService 'ServerB)                 
  -- one of these has to not return, otherwise the program will exit
  runServer client

