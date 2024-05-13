
module Main where
import System.Environment (getArgs)
import Domain (getWadIds, WadId, getServer, getWadPackId)
import WadFiles (existsWadFile, unzipFile)
import Control.Monad (filterM)
import WadloaderParser (parseUrlAction)
import GHC.Base (failIO)
import WadDownload (downloadWadPack)
-- Exported from the package conduit-extra

--TODO:
-- execute start{id}.cmd file
-- get server from Action
-- maybe: cleanup z.zip files
main :: IO ()
main = do
  args <- getArgs
  action <- eitherToIO $ do
    arg <- handleArgs args
    parseUrlAction arg
  downloadIds <- filterWads $ getWadIds action 
  path <- downloadWadPack (getServer action) (getWadPackId action) downloadIds
  unzipFile path

handleArgs :: [String] -> Either String String
handleArgs [] = Left "No Args given"
handleArgs (x:_) = Right x

eitherToIO :: Either String a -> IO a
eitherToIO (Left msg) = failIO msg
eitherToIO (Right wadIds) = return wadIds

filterWads :: [WadId] -> IO [WadId]
filterWads = filterM (fmap not . existsWadFile ) 
