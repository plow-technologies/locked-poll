import LockedPollSpec 
import Test.Tasty
import System.Directory
import System.IO.Error 

removeTestFiles :: IO ()
removeTestFiles = flip catchIOError (\e -> if isDoesNotExistError e then return () else ioError e) $ do
  removeFile shouldBreakFile
  removeFile shortNoBreakFile
  removeFile noBreakFile

main :: IO ()
main = do
  removeTestFiles
  defaultMain tests

  removeTestFiles
