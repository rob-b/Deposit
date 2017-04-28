{-# LANGUAGE OverloadedStrings #-}

module Deposit where

import           Data.Aeson                           hiding (json)
import qualified Data.ByteString                      as B
import           Data.CaseInsensitive                 (original)
import           Data.List                            (intercalate)
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           GHC.Exts
import           Network.Socket                       (SockAddr (SockAddrInet),
                                                       hostAddressToTuple)
import           Network.Wai                          (Request, queryString,
                                                       remoteHost,
                                                       requestHeaders)
import           Network.Wai.Middleware.RequestLogger
import           System.Environment                   (lookupEnv)
import           Web.Spock.Core


type DepositAction a = ActionCtxT () IO a


main :: IO ()
main = do
  port <- fmap (fromMaybe "8080") (lookupEnv "PORT")
  putStrLn $ "Ready to take deposits on port " ++ port
  runSpock (read port) (spockT id (mw >> app))


app :: SpockT IO ()
app = do
  get root . json $ decodeUtf8 "welcome to deposit, pal"
  get "get" $
    do req <- request
       json $ combinedGetInfo req
  post "post" $
    do req <- request
       contentTypeM <- header "Content-Type"
       case contentTypeM of
         Nothing -> undefined
         Just contentType -> selectResponseKind contentType req


selectResponseKind :: T.Text -> Request -> DepositAction ()
selectResponseKind contentType req =
  if contentType == "application/json"
    then jsonAction req
    else formAction req


formAction :: Request -> DepositAction ()
formAction req = do
  payload <- body
  json $ combinedPostInfo req payload

jsonAction :: Request -> DepositAction ()
jsonAction req = do
  payload <- jsonBody'
  body' <- body
  json . Object $
    fromList
      [ "json" .= (payload :: Value)
      , "headers" .= headerInfo req
      , "origin" .= originInfo req
      , "args" .= queryStringInfo req
      , "data" .= decodeUtf8 body']


mw :: SpockT IO ()
mw = middleware logStdoutDev


-- | Combine the various info sources about a POST request
combinedPostInfo :: Request -> B.ByteString -> Value
combinedPostInfo req body' =
  Object $
  fromList
    [ "headers" .= headerInfo req
    , "origin" .= originInfo req
    , "args" .= queryStringInfo req
    , "data" .= decodeUtf8 body']


-- | Combine the various info sources about a GET request
combinedGetInfo :: Request -> Value
combinedGetInfo req =
  Object $
  fromList
    [ "headers" .= headerInfo req
    , "origin" .= originInfo req
    , "args" .= queryStringInfo req]


-- | Info about the headers sent with the request
headerInfo :: Request -> Value
headerInfo req = Object $ fromList (headers req)
  where
    headers :: Request -> [(T.Text, Value)]
    headers req' =
      map
        (\(k,v) -> (decodeUtf8 (original k), String $ decodeUtf8 v))
        (requestHeaders req')


-- | Info about the source ip of the request
originInfo :: Request -> Value
originInfo req = trans $ remoteHost req
  where
    trans (SockAddrInet _ hostAddr) = toIp $ hostAddressToTuple hostAddr
    trans _ = "n/a"
    toIp (a,b,c,d) =
      String . T.pack $ intercalate "." [show a, show b, show c, show d]


-- | Info about the querystring parameters
queryStringInfo :: Request -> Value
queryStringInfo req =
  let tupleToValue (k,v) = decodeUtf8 k .= map decodeUtf8 v
      groupedValues = foldl groupValuesWithKey [] (queryString req)
  in Object . fromList $ map tupleToValue groupedValues


-- | Given a list of pairs like [(a, 1), (a, 2)], group the values by the key - [(a, [1, 2])]
groupValuesWithKey
  :: (IsString a, Eq a1)
  => [(a1, [a])] -> (a1, Maybe a) -> [(a1, [a])]
groupValuesWithKey old (k,v) =
  let newValue = fromMaybe "" v
  in case lookup k old of
       Nothing -> (k, [newValue]) : old
       Just v' -> (k, newValue : v') : filter (\pair -> fst pair /= k) old
