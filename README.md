# locked-poll


```locked-poll```  is a package to enable simple concurrent polling with a locking scheme.

The library is quite small, needing
* ```containers```
* ```clock```

To run.

There are lots of tests in the test-suite to ensure that it behaves itself with polls.



## Usage-Example

There is only one function :

``` haskell

makeLockingFunction :: forall k lockableState . (Key k,Show lockableState) =>
                       Int64 ->  -- timeout
                       (KeyFcn lockableState k) ->          -- extract a key
                       IO ((lockableState -> IO ()) ->      -- function to run against state
                       lockableState   ->                   -- incoming state
                       IO ())

```

This function is used to generate a function which has an embedded action that is dependent on some given state.
The idea is to generate that locking function at the top level and then pass it to whatever needs incremental access to a resource.

It uses an IORef under the hood to store what states are currently in use.

Use the timeout function to say when to release the lock.

No cleanup is performed by the lockingFunction, so wrap your things in finally or bracket as needed.


``` haskell
-- | 'Key' Constraint for locking down a traverse or fold
type Key k = (Ord k,Eq k,Show k)

-- |Extract a key from a given state
type KeyFcn st k = Key k => st -> k

```

KeyFcn tells the lockingFunction how to extract the LockingKey.

Uniqueness is not necessary but if you do not have it, only one of a given group of like Keyed objects will be allowed to run at a time.



## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

The tests are quite slow (4 or 5 min) because they are trying to test long poll concurrency

Right now you have to hand delete the resulting output.

## Contributing

TODO: Write contribution instructions here
