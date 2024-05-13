{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
module WadFiles where
import Domain (WadId, wadloaderPath, wadPath, wadSegment)

import System.Directory (doesFileExist, doesDirectoryExist, createDirectory, getAppUserDataDirectory)
import Control.Monad (unless)
import System.FilePath (pathSeparator)

existsWadFile :: WadId -> IO Bool
existsWadFile fileName = do
  dirPath <- getAppUserDataDirectory $ wadPath ++ (pathSeparator : show fileName)
  print dirPath
  doesFileExist dirPath

createWadDirIfNotExists :: IO ()
createWadDirIfNotExists = do
  dirPath <- getAppUserDataDirectory wadloaderPath
  existsLoaderPath <- doesDirectoryExist dirPath
  unless existsLoaderPath $ createDirectory dirPath
  existsWadPath <- doesDirectoryExist $ dirPath ++ wadSegment 
  unless existsWadPath $ createDirectory $ dirPath ++ wadSegment


