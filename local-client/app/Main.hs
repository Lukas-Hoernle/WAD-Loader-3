{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Exported from the package conduit-extra

import Conduit (runConduit, runResourceT, (.|))
import Data.Aeson
import Data.Conduit.Binary (sinkFile)
import Network.HTTP.Conduit

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

wads :: [Wad]
wads = [Wad "Wad1" 1, Wad "Wad2" 2]

main :: IO ()
main = do
  manager <- newManager tlsManagerSettings
  initRequest <- parseRequest "POST http://httpbin.org/post"
  let request =
        initRequest
          { requestBody = RequestBodyLBS $ encode wads
          , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
          }

  runResourceT $ do
    response <- http request manager
    runConduit $ responseBody response .| sinkFile "foo.txt"
