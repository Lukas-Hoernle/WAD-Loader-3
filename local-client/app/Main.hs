
module Main where
import System.Environment (getArgs)
import Domain (getWadIds, WadId, getServer, getWadPackId)
import WadFiles (existsWadFile)
import Control.Monad (filterM)
import WadloaderParser (parseUrlAction)
import GHC.Base (failIO)
import WadDownload (downloadWadPack)
-- Exported from the package conduit-extra

--TODO:
--check how to use Envars
--check how to download files from my api
--implement wadloader protoc ol
--chose values from paths to store wadpacks and wads (one for both otherwise the template wont work)
--
--templates are named {id}.bat wads are only named {id}

main :: IO ()
main = do
  args <- getArgs
  action <- eitherToIO $ do
    arg <- handleArgs args
    parseUrlAction arg
  downloadIds <- filterWads $ getWadIds action 
  downloadWadPack (getServer action) (getWadPackId action) downloadIds 
  print downloadIds

    --(show . parse urlAction $ x)
    --
handleArgs :: [String] -> Either String String
handleArgs [] = Left "No Args given"
handleArgs (x:_) = Right x

eitherToIO :: Either String a -> IO a
eitherToIO (Left msg) = failIO msg
eitherToIO (Right wadIds) = return wadIds

filterWads :: [WadId] -> IO [WadId]
filterWads = filterM (fmap not . existsWadFile ) 
