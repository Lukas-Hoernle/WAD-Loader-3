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

data Wad where
  Wad :: String -> Int -> Wad

instance ToJSON Wad where
  toJSON (Wad name wadId) =
    object
     [ "name" .= name
     , "wadId" .= wadId
     ]

wads2 :: [Wad]
wads2 = [Wad "Wad1" 1, Wad "Wad2" 2]

downloadWadPack :: DownloadUrl -> WadPackId -> [WadId] -> IO () 
downloadWadPack server packId wads = do
  url <- return $ wadPackApiPath server packId 
  print url
  manager <- newManager tlsManagerSettings 
  initRequest <- parseRequest $ "POST " ++ url
  let request =
        initRequest
        --todo check if wads is encoded properly
          { requestBody = RequestBodyLBS $ encode wads
          , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }
  
  dir <- getAppUserDataDirectory wadloaderPath
  createWadDirIfNotExists 
  runResourceT $ do
    response <- http request manager
    runConduit $ responseBody response .| sinkFile (dir ++ wadSegment ++ [pathSeparator] ++ show packId ++ ".zip")
  

downloadFileGET :: IO ()
downloadFileGET = do
  manager <- newManager tlsManagerSettings
  initRequest <- parseRequest "POST http://httpbin.org/post"
  let request =
        initRequest
          { requestBody = RequestBodyLBS $ encode wads2
          , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  runResourceT $ do
    response <- http request manager
    runConduit $ responseBody response .| sinkFile "foo.txt"
