{-# LANGUAGE OverloadedStrings #-}
module WadFiles where
import Domain (WadId, wadloaderPath, wadPath, wadSegment)

import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, getAppUserDataDirectory)
import Control.Monad (unless)
import System.FilePath (pathSeparator, takeDirectory)
import Codec.Archive.Zip
import qualified Data.ByteString.Lazy as BSL

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
