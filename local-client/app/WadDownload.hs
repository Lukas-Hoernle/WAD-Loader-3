{-# LANGUAGE OverloadedStrings #-}

module WadDownload where

import System.FilePath (pathSeparator)
import System.Directory (getAppUserDataDirectory)
import Conduit (runConduit, runResourceT, (.|))
import Data.Aeson 
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit
import Domain (WadId, WadPackId, DownloadUrl, wadPackApiPath, wadloaderPath, wadSegment)
import WadFiles (createWadDirIfNotExists) 
-- https://hackage.haskell.org/package/http-conduit-2.3.8.3/docs/Network-HTTP-Conduit.html
-- https://github.com/snoyberg/http-client/blob/master/TUTORIAL.md

downloadWadPack :: DownloadUrl -> WadPackId -> [WadId] -> IO () 
downloadWadPack server packId wads = do
  url <- return $ wadPackApiPath server packId wads
  print url
  manager <- newManager tlsManagerSettings 
  initRequest <- parseRequest $ "GET " ++ url
  let request =
        initRequest
          { 
           requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }
  
  dir <- getAppUserDataDirectory wadloaderPath
  createWadDirIfNotExists 
  runResourceT $ do
    response <- http request manager
    runConduit $ responseBody response .| sinkFile (dir ++ wadSegment ++ [pathSeparator] ++ show packId ++ ".zip")
