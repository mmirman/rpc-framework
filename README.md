rpc-framework
=============

This is a framework for remote procedure calls in haskell.

Features
--------
* Usage is incredibly simple!

* Calling a remote procedure is type safe.

* Modal logic inspired worlds, aka hosts:
    * Services run from the _World IO_ monad, written `Host w => WIO w`
    * This allows world specific actions: if one world will be compiled to JS and one to x86, we could have the following types

```haskell
    putStrLn :: IO_World w => WIO w a
    installTextBox :: JS_World w => WIO w ()
```

* Arbitrily complex remote procedures:
    * Rather than only being able to call a remote function of one argument, we can call with any number of arguments

```haskell
    foo :: (Sendable a1 ,..., Sendable aN, Sendable b, Host w) => a1 -> ... -> aN -> WIO w b
```

* It can _send_ _pure_ functions across the wire and now garbage collect them.

```haskell 
    instance (Serializable a) => Sendable a a
    instance (Sendable a' a, Sendable b b') => Sendable (a -> b) (a' -> IO b')
```

* Only local code can execute
    * unlike some modal logic aproaches to mobile languages, the only code that can be executed is the code you compiled, and not code passed from world to world

* State can be serialized with references.

Usage
-----

* To install, run cabal install

* Hosts are declared at the declaration level
```haskell
    $(makeHost "WorldName" "host_location" #portNumber)
```
* Installing a remote service: 
   * `makeServices` registers a list of service names which all have the same host 
   * `autoService` automatically figures out which services in the given file run on the specified host and registers them.  A good usage pattern is to provide all modules with services with a registration hook that can be appended to the main server.

```haskell
    main = runServer $(makeServices [ 'nameOfService1, ... , 'nameOfServiceN])
```

```haskell
    main = runServer $(autoService 'HostName)
```

```haskell
    module First where
    services = $(autoService 'HostName)

    module Second where
    services = $(autoService 'HostName)

    module Main where
    import qualified First as F
    import qualified Second as S
    main = runServer $ F.services >> S.services
```

* Calling a remote service:
```haskell
    addServer :: Integer -> WIO Server (Integer -> Integer)
    addServer t = do
        Server <- world
	return (t +)

    getRemoteAdd = $(rpcCall 'addServer)
    ...
    ghci> :t getRemoteAdd
    getRemoteAdd  :: Host w => Integer -> WIO w (Integer -> Integer)
```

Examples
--------

* src/RPCTest.hs is an example.  
    * It runs two worlds, Client and Server, both on localhost, one on port 9000 and the other on 9001.
    * To run and build it:  `make run`
    * To build it: `make test`
    * To run it after building it: `./rpc-test`




