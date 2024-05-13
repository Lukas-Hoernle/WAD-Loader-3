module Domain where

import System.FilePath (pathSeparator)

wadloaderPath :: String
wadloaderPath = "wadloader3"
wadSegment :: [Char]
wadSegment = pathSeparator :"local"
wadPath :: String
wadPath = wadloaderPath ++ wadSegment 

wadPackApiPath :: String -> WadPackId -> String
wadPackApiPath url packId = url ++ "/download/wadpack/" ++ show packId

type WadId = Int
type WadPackId = Int
type DownloadUrl = String

data Action where
  DownloadWadPack :: WadPackId -> [WadId] -> DownloadUrl -> Action
  DownloadAndStartWadPack :: WadPackId -> [WadId] -> DownloadUrl -> Action

getWadPackId :: Action -> WadPackId
getWadPackId (DownloadWadPack packId _ _) = packId
getWadPackId (DownloadAndStartWadPack packId _ _) = packId

getWadIds :: Action -> [WadId]
getWadIds (DownloadWadPack _ wadIds _) = wadIds
getWadIds (DownloadAndStartWadPack _ wadIds _) = wadIds

getServer :: Action -> DownloadUrl
getServer (DownloadWadPack _ _ server) = server
getServer (DownloadAndStartWadPack _ _ server) = server

instance Show Action where
  show :: Action -> String
  show action = case action of
    DownloadWadPack packid wads server -> "DownloadWadPack " ++ show packid ++ " " ++ show wads ++ " " ++ server
    DownloadAndStartWadPack packid wads server ->  "DownloadAndStartWadPack " ++ show packid ++ " " ++ show wads ++ " " ++ server
