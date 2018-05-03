import LockedPollSpec 
import Test.Tasty
import System.Directory
import System.IO.Error 

removeFileIfNotExists :: FilePath -> IO ()
removeFileIfNotExists filePath =
  catchIOError
    (removeFile filePath)
    (\e -> if isDoesNotExistError e then return () else ioError e)

removeTestFiles :: IO ()
removeTestFiles = do
  removeFileIfNotExists shouldBreakFile
  removeFileIfNotExists shortNoBreakFile
  removeFileIfNotExists noBreakFile

main :: IO ()
main = do
  removeTestFiles
  defaultMain tests

  removeTestFiles
