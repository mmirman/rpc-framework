{- |
Module      :  Network.Remote.RPC
Copyright   :  (c) Matthew Mirman 2012
License     :  BSD-style (see the file LICENSE)
Maintainer  :  Matthew Mirman <mmirman@andrew.cmu.edu>
Stability   :  experimental
Portability :  Many GHC only extensions

Declaration of the frontend for RPC.

-}

module Network.Remote.RPC ( Host(..)
                          , WIO()
                          , world
                          , Servable()
                          , liftIO
                          , runServer
                          , runServerBG
                          , onHost
                          , autoService
                          , makeHost
                          , makeServices
                          , rpcCall
                          , Forkable(..)
                          -- * Example: Making remote Calls
                          -- $remoteExample
                          , MonadTrans(..)
                          ) where

import Network.Remote.RPC.Internal.Runtime
import Network.Remote.RPC.Internal.Templates
import Control.Concurrent.ForkableRPC
import Control.Monad.Trans.Class ( MonadTrans(..))

{- $remoteExample
This example shows how to make remote procedure from a Client to a Host.  
It also shows how to send functions, which can now be collected.

>-- LANGUAGE TemplateHaskell, KindSignatures, FlexibleContexts
>module Main where
>import Network.Remote.RPC

First, hosts must be declared.   
In the future, hosts might be declared in a configuration file such that
they can be configured at runtime rather than only at compile time. 

@
$('makeHost' \"Client\" \"localhost\" 9000)
$('makeHost' \"Server\" \"localhost\" 9001)
@

The following services will run on the server.

@
doubleServer :: Integer -> 'WIO' Server IO Integer
doubleServer t = return $ t + t

addServer :: Integer -> 'WIO' Server IO (Integer -> Integer)
addServer t = return (t +)
@

When used, @addServer@ will return a function of type @Integer -> 'WIO' w IO Integer@
because the resulting function will actually be a remote call.  Every time @addServer@
is called, it turns the function into a service which is collected when the function is no  
longer needed.

>client = do
>  onHost Client
>  double <- $(rpcCall 'doubleServer) 3
>  liftIO $ putStrLn $ "r(h3+h3) ? " ++ show double
>         
>  add <- $(rpcCall 'addServer) 4
>  r <- add 6 -- add :: Integer -> WIO Client IO Integer
>  liftIO $ putStrLn $ "r(h4 +) h6 ? " ++ show r

Despite 'rpcCall' being a template splice, the resulting splice is type safe:   

@ $('rpcCall' 'doubleServer) :: ('Host' w, 'Servable' m) => Integer -> 'WIO' w m Integer @


Now we declare the runtime.  Usually this would be in two different mains, but for 
educational and testing purposes, both the client and server can be run from the same main.

@
main = do
  'runServerBG' $(autoService 'Server)
  'runServer' client
@

(1) @$('autoService' 'Server)@ looks for services declared in the file which definitely run on server, and 
 runs them as services on the intended host (@Server@ in this case).

(1) 'runServerBG' runs a service server on a background OS thread, and returns 

(1) 'runServer' runs a service server and does not return.

(1) When run:
  
>>> :main
r(h3+h3) ? 6
r(h4 +) h6 ? 10

Where @rVal@ means Val is on the server and @hVal@ means Val is on the client. 
-}

