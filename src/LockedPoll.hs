{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{- |
Module      : LockedPoll
Description : Lock an item while polling it for a given period of time
Copyright   : Plow Technologies LLC
License     : MIT License

Maintainer  : Scott Murphy
>>> let keyFcn = fst :: (Int,a) -> Int
>>> lockingFunction <- makeLockingFunction 30 keyFcn

| -}



-- module LockedPoll ( makeLockingFunction
--                  , KeyFcn ) where

module LockedPoll (Key,KeyFcn,makeLockingFunction) where

import           Control.Exception                (bracket)
import           Data.IORef
import Data.Int (Int64)
import           Data.Monoid                      ((<>))
import           System.IO
import qualified Data.Map                         as Map
import           Data.Map.Strict                  (Map)
import           System.Clock                     (Clock (..), TimeSpec (..), getTime)



-- | 'Key' Constraint for locking down a traverse or fold
type Key k = (Ord k,Eq k,Show k)

-- |Extract a key from a given state
type KeyFcn st k = Key k => st -> k

-- | Patterns to make what is going on in the locking more clear
-- They have the unfortunate side effect of requiring a match against bottom

type LockStatus = Maybe Bool
pattern KeyAcquired :: LockStatus
pattern KeyAcquired <-  Just True
pattern IOLocked :: LockStatus
pattern IOLocked <- Just False
pattern LockTimedOut :: LockStatus
pattern LockTimedOut <- Nothing

-- | This locking Function is very simple
-- It reads a key and assigns a time to it
-- if that time has expored... then it unlocks
-- if the nested action completes then it unlocks
-- all concurrency and exception handling should be done inside the IO ()
-- Notice I say it unlocks, not that it kills a nascent thread.  No such thing will happen automatically

makeLockingFunction :: forall k lockableState . (Key k,Show lockableState) =>  Int64 ->  -- timeout
                       (KeyFcn lockableState k) ->          -- extract a key
                       IO ((lockableState -> IO ()) ->      -- function to run against state
                           lockableState   ->                   -- incoming state
                           IO ())
makeLockingFunction timeout getKey = do
  ioRefMapOfKeyValues <- newIORef  Map.empty :: IO (IORef (Map k TimeSpec) )
  return $ exportFunction ioRefMapOfKeyValues
 where   
   unlock :: IORef (Map k TimeSpec) -> k -> IO ()
   unlock ioRef k = atomicModifyIORef' ioRef (\map' -> (Map.delete k map', ()))

   obtainLock :: (Key k) =>  TimeSpec -> k -> Map k TimeSpec -> (Map k TimeSpec ,Maybe Bool)
   obtainLock lockTime@(TimeSpec s1 _) k map' = case Map.lookup k map' of
                   Just (TimeSpec s _)
                       | abs s1 - s >= timeout ->  (Map.insert k lockTime map', Nothing) --old value timed out, aquire new lock assume old thread be dead
                       | otherwise -> (map',Just False) --Lock not obtained
                   Nothing ->  (Map.insert k lockTime map',Just True) --acquire brand spanking new lock

   exportFunction :: IORef (Map k TimeSpec) ->
                     (lockableState -> IO ()) -> lockableState -> IO ()
   exportFunction ioRef f st  = bracket ioLock ioUnlock runFunction
    where
      ioLock :: IO (Maybe Bool)
      ioLock = do
        lockTime <- getTime Monotonic
        atomicModifyIORef'  ioRef . obtainLock lockTime . getKey $ st

      ioUnlock :: LockStatus -> IO ()
      ioUnlock lockStatus = case lockStatus of
                              IOLocked -> return ()
                              KeyAcquired -> unlock ioRef (getKey st)
                              LockTimedOut  ->  unlock ioRef (getKey st)
                              _ -> error "no match for bottom in ioUnlock"

      runFunction ::  LockStatus -> IO ()
      runFunction iCanRun = case iCanRun of
                              KeyAcquired -> f st
                              IOLocked -> hPrint stderr ("skipping locked thread" <> (show . getKey $ st ))
                              LockTimedOut -> hPrint stderr ("thread timeout " <> show st ) >> f st
                              _ -> error "no match for bottom of Maybe Bool in makeLockingFunction"
