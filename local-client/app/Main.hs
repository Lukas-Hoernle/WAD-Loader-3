
module Main where
import System.Environment (getArgs)
import Domain (getWadIds, WadId, getServer, getWadPackId, startAction)
import WadFiles (existsWadFile, unzipFile, runWadPack)
import Control.Monad (filterM, when)
import WadloaderParser (parseUrlAction)
import GHC.Base (failIO)
import WadDownload (downloadWadPack)

--TODO:
-- execute start{id}.cmd file
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
  when (startAction action) $ runWadPack (getWadPackId action) 

handleArgs :: [String] -> Either String String
handleArgs [] = Left "No Args given"
handleArgs (x:_) = Right x

eitherToIO :: Either String a -> IO a
eitherToIO (Left msg) = failIO msg
eitherToIO (Right wadIds) = return wadIds

filterWads :: [WadId] -> IO [WadId]
filterWads = filterM (fmap not . existsWadFile ) 
