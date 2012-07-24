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

    * It can also _send_ pure functions across the wire **note** this really sets up a service and the sent function will persist indefinitely, and thus is slow.  This also means that the function received isn't really pure

```haskell 
    instance (Serializable a) => Sendable a
    instance (Sendable a, Sendable b) => Sendable (a -> b)
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
```haskell
    main = runServer $(makeServices [ 'addServer , 'doubleServer])
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

* src/RPCTest.hs contains a worked example.  
    * To run and build it:  `make run`
    * To build it: `make test`
    * To run it after building it: `./test`




