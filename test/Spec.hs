import LockedPollSpec 
import Test.Tasty
import System.Directory


removeTestFiles = do
  removeFile shouldBreakFile
  removeFile shortNoBreakFile
  removeFile noBreakFile

main :: IO ()
main = do
  removeTestFiles
  defaultMain tests

  removeTestFiles
