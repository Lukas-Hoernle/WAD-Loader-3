module WadFiles where

import Domain (WadId, wadPath, wadSegment, wadloaderPath, WadPackId)
import System.Process (system)
import Codec.Archive.Zip (
  ZipOption (OptDestination, OptRecursive, OptVerbose),
  extractFilesFromArchive,
  toArchive,
 )
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as BSL
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, getAppUserDataDirectory)
import System.FilePath (pathSeparator, takeDirectory)

existsWadFile :: WadId -> IO Bool
existsWadFile fileName = do
  dirPath <- getAppUserDataDirectory $ wadPath ++ (pathSeparator : show fileName)
  doesFileExist dirPath

createWadDirIfNotExists :: IO ()
createWadDirIfNotExists = do
  dirPath <- getAppUserDataDirectory wadloaderPath
  existsLoaderPath <- doesDirectoryExist dirPath
  unless existsLoaderPath $ createDirectory dirPath
  existsWadPath <- doesDirectoryExist $ dirPath ++ wadSegment
  unless existsWadPath $ createDirectory $ dirPath ++ wadSegment

unzipFile :: String -> IO ()
unzipFile strPath =
  do
    byteString <- BSL.readFile strPath
    let archive = toArchive byteString
    extractFilesFromArchive [OptRecursive, OptVerbose, OptDestination $ takeDirectory strPath] archive

runWadPack :: WadPackId -> IO ()
runWadPack packId = do
  dirPath <- getAppUserDataDirectory wadPath 
  let command = "start " ++ dirPath ++ pathSeparator:"start" ++ show packId ++ ".cmd"
  print command
  _ <- system command
  return ()

