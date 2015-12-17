{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module LockedPollSpec (tests) where
import qualified Data.Ord                         as Ord

import           Control.Concurrent

import           Data.Int                         (Int64)

import           LockedPoll
import           Test.Tasty
import           Test.Tasty.HUnit

import           Control.Lens
import           Control.Monad
import           Data.Either                      (lefts)
import qualified Data.List                        as List
import           Data.Monoid                      ((<>))
import           Data.Time.Clock.POSIX
import           System.IO
import           System.Random

import           Data.Attoparsec.ByteString.Char8

import qualified Data.ByteString                  as BS

-- --------------------------------------------------
-- Exportable Tests
--------------------------------------------------
tests :: TestTree
tests = testGroup "Locking Properties" [unitTests]

-- qcprops :: TestTree
-- qcprops = _


unitTests :: TestTree
unitTests = testGroup "Unit tests for lock stuff and other tests" [ testTests
                                                                  , longTimeoutShouldWork
                                                                  , shortTimeoutShouldBreak]
 where
  testTests = testCase "make sure a bad file is a failure" $ assert (not <$> fullTestShouldBreak )
  longTimeoutShouldWork = testCase "a good file and reasonable timeout succeeds" $ assert (fullTestWithLock 300 )
  shortTimeoutShouldBreak = testCase "a good file and short timeout still fails" $ assert (not <$> fullTestWithLock 1)


keyList :: [(Int, POSIXTime)]
keyList = [(1,0),(2,0),(3,0),(4,0)]

-- REMOVE THIS NEXT!!
-- fileName :: FilePath
-- fileName = "incremental-test-file.txt"

shouldBreakFile :: FilePath
shouldBreakFile = "incremental-test-file-should-break.txt"

noBreakFile :: FilePath
noBreakFile = "incremental-test-file-no-break.txt"

fullTestShouldBreak :: IO Bool
fullTestShouldBreak = do
   writeToFileIncrementally shouldBreakFile
   checkResults shouldBreakFile

fullTestWithLock :: Int64 -> IO Bool
fullTestWithLock time = do
   writeToFileIncrementallyWithlock time noBreakFile
   putStrLn "Done Writing Test File"
   checkResults noBreakFile

writeToFileIncrementallyWithlock :: Int64 -> FilePath -> IO ()
writeToFileIncrementallyWithlock time fileName = do
  putStrLn "making lock function"
  lockingFunction <- makeLockingFunction time fst

  withFile fileName  AppendMode (\handle -> do
     let
        ioAction :: (Int, POSIXTime) -> IO ()
        ioAction key = threadDelay (5 * multiplier) >> waitThenWriteTheTime lockingFunction handle key
     putStrLn "preparing to replicate"
     replicateM_ repitition $ ioAction  `traverse` keyList
     threadDelay (two *maxVal*( multiplier)) )

  where
    two = 2
    repitition = 5 * two
    maxVal = 100
    waitThenWriteTheTime lockingFunction handle st = do
                   putStrLn "getting Time"
                   currentTime <- getPOSIXTime
                   delayTime <- (* multiplier) <$> randomRIO (5,maxVal)
                   let
                     lockableAction (k ,_) = threadDelay delayTime >>
                                             nicelyFormattedPrint handle currentTime k
                     withLockedAction = lockingFunction lockableAction (st & _2 .~ currentTime)
                   _ <- forkIO $ withLockedAction
                   return ()

    multiplier = 10^(4::Integer)
    nicelyFormattedPrint handle currentTime k = do
                         putStrLn (show k)
                         hPutStrLn handle $ show k <> "|"<> show currentTime


writeToFileIncrementally :: FilePath -> IO ()
writeToFileIncrementally fileName = do
  withFile fileName  AppendMode (\handle -> do
     replicateM_ repitition $ threadDelay (15 * multiplier) >> (waitThenWriteTheTime handle `traverse` keyList)
     threadDelay (repitition*maxVal*multiplier))

  where
    repitition = 10
    maxVal = 100
    waitThenWriteTheTime :: (Show a) => Handle -> (Int,a) -> IO ()
    waitThenWriteTheTime handle (k,_) = do
                   currentTime <- getPOSIXTime
                   delayTime <- (* multiplier) <$> randomRIO (5,maxVal)
                   _ <- forkIO $ threadDelay delayTime >> nicelyFormattedPrint handle currentTime k
                   return ()

    multiplier = 10^(3::Integer)
    nicelyFormattedPrint handle currentTime k  = hPutStrLn handle $ show k <> "|"<> show currentTime



-- Check Your Self
checkResults :: FilePath -> IO Bool
checkResults fileName = do
   rslt <- getResults fileName
   if null $ lefts . fmap strictlyIncreasing . keyGroup $ rslt
   then return True
   else do
     print $ fmap strictlyIncreasing . keyGroup $ rslt
     return False
 where
  keyEq :: forall a k. (Key k) => (k,a) -> (k,a) -> Bool
  keyEq = (\a0 a1 -> fst a0 == fst a1)
  keyGroup :: forall a k. (Key k) => [(k,a)] -> [[(k,a)]]
  keyGroup rslt = List.groupBy keyEq . List.sortBy (Ord.comparing fst) $ rslt
  strictlyIncreasing :: (Ord a,Eq a,Show a, Show k,Eq k) => [(k,a)] -> Either String (k,a)
  strictlyIncreasing ((k0,a0):lst) = List.foldl' increasingStep (Right (k0,a0))  lst
  strictlyIncreasing [] = Left "empty list!"
  increasingStep :: (Ord a,Eq a,Show a, Show k,Eq k) => Either String (k,a) -> (k,a) -> Either String (k,a)
  increasingStep (Right (k0,a0)) (k1,a1)
     | k1 == k0 && a1 >= a0 = Right (k1,a1)
     | otherwise = Left $ "list not strictly increasing in key " <> (show k1) <> " with value " <> (show a1) <>
                          "is preceded by key " <> (show k0) <> " with value " <> (show a0)
  increasingStep l _ = l

parseResults :: Parser [(Int,Double)]
parseResults =  many' tupleParser
  where
    tupleParser = do
           i <- decimal
           _ <- char '|'
           d <-  double
           _ <- char 's'
           _ <- endOfLine
           return (i,d)

getResults :: FilePath ->IO [(Int, Double)]
getResults fileName = do
  bs <- BS.readFile fileName
  case (parseOnly parseResults bs) of
   (Left err) -> fail ("error parsing, here is the residual" <> err)
   (Right rslt) -> return rslt
