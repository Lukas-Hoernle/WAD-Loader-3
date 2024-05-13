module WadloaderParser where
import Parser (Parser (parse), tokenMatching, many', result, zero, then')
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Domain (Action (DownloadWadPack, DownloadAndStartWadPack))
import Control.Applicative (Alternative(..))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)

wadpackDownload :: String
wadpackDownload = "wadpack"
wadpackDownloadStart :: String
wadpackDownloadStart = "startwadpack"
delimiter :: Char
delimiter = ','
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
    ns <- many' (tokenMatching (==delimiter) >> number)
    return (n:|ns)
--TODO add real parser for server url
actionConstructor :: Parser Char (NonEmpty Int -> Action)
actionConstructor = 
  (string wadpackDownload >> result (\(x:|xs) -> DownloadWadPack x xs "http://localhost:8080")) <|> 
  (string wadpackDownloadStart >> result (\(x:|xs) -> DownloadAndStartWadPack x xs "http://localhost:8080"))

action :: Parser Char Action
action = then' ($) actionConstructor numbers

urlAction :: Parser Char Action
urlAction = string urlPrefix >> action

parseUrlAction :: String -> Either String Action
parseUrlAction arg = maybe 
  (Left $ "Could not parse: " ++ arg)
  (\((x,rest):|_) -> if rest == "" 
                     then Right x 
                     else Left $ "Could not parse, unexpected: " ++ rest)
  (nonEmpty $ parse urlAction arg) 
