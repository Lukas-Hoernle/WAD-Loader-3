module WadloaderParser where

import Control.Applicative (Alternative (..))
import Data.Char (isDigit)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Domain (Action (DownloadAndStartWadPack, DownloadWadPack), DownloadUrl)
import Parser (Parser (parse), many', result, then', tokenMatching, zero, remainder)
import Text.Read (readMaybe)

wadpackDownload :: String
wadpackDownload = "wadpack"
wadpackDownloadStart :: String
wadpackDownloadStart = "startwadpack"
delimiter :: Char
delimiter = '-'
urlPrefix :: String
urlPrefix = "wadloader://"

string :: String -> Parser Char String
string = foldr (then' (:) . tokenMatching . (==)) (result "")

digitC :: Parser Char Char
digitC = tokenMatching isDigit

digitsC :: Parser Char String
digitsC = many' digitC

number :: Parser Char Int
number = digitsC >>= (\chars -> maybe zero result (readMaybe chars :: Maybe Int))

numbers :: Parser Char (NonEmpty Int)
numbers =
  do
    n <- number
    ns <- many' (tokenMatching (== delimiter) >> number)
    return (n :| ns)

actionConstructor :: Parser Char (NonEmpty Int -> DownloadUrl -> Action)
actionConstructor =
  (string wadpackDownload >> result (\(x :| xs) url -> DownloadWadPack x xs url))
    <|> (string wadpackDownloadStart >> result (\(x :| xs) url -> DownloadAndStartWadPack x xs url))

--this assumes the remaining string is a server url (with protocol etc.)
action :: Parser Char Action
action = actionConstructor <*> numbers <*> (string "$" >> remainder)

urlAction :: Parser Char Action
urlAction = string urlPrefix >> action

--expects: {action}{packId},{wadIds}${url}
-- action is either wadpack or startwadpack
-- packId is an positive Integer
-- wadIds is a '-' separated string of positive Integers
-- url is the url to send the request to
parseUrlAction :: String -> Either String Action
parseUrlAction arg =
  maybe
    (Left $ "Could not parse: " ++ arg)
    ( \((x, rest) :| _) ->
        if rest == ""
          then Right x
          else Left $ "Could not parse, unexpected: " ++ rest
    )
    (nonEmpty $ parse urlAction arg)
