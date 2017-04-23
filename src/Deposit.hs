{-# LANGUAGE OverloadedStrings #-}

module Deposit where

import           Data.Aeson                           hiding (json)
import           Data.CaseInsensitive                 (original)
import qualified Data.ByteString                      as B
import           Data.List                            (intercalate)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           GHC.Exts
import           Network.Socket                       (SockAddr (SockAddrInet),
                                                       hostAddressToTuple)
import           Network.Wai                          (Request, queryString,
                                                       remoteHost,
                                                       requestHeaders)
import           Network.Wai.Middleware.RequestLogger
import           Web.Spock.Core


-- {
--   "args": {},
--   "data": "{\"a\": 123}",
--   "files": {},
--   "form": {},
--   "headers": {
--     "Accept": "*/*",
--     "Connection": "close",
--     "Content-Length": "10",
--     "Content-Type": "application/json",
--     "Host": "httpbin.org",
--     "User-Agent": "curl/7.51.0"
--   },
--   "json": {
--     "a": 123
--   },
--   "origin": "212.69.50.106",
--   "url": "http://httpbin.org/post"
-- }


main :: IO ()
main = runSpock 8080 (spockT id (mw >> app))


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

type DepositAction a = ActionCtxT () IO a

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
      , "args" .= getInfo req
      , "data" .= decodeUtf8 body']


mw :: SpockT IO ()
mw = middleware logStdoutDev


combinedPostInfo :: Request -> B.ByteString -> Value
combinedPostInfo req body' =
  Object $
  fromList
    [ "headers" .= headerInfo req
    , "origin" .= originInfo req
    , "args" .= getInfo req
    , "data" .= decodeUtf8 body']


combinedGetInfo :: Request -> Value
combinedGetInfo req =
  Object $
  fromList
    [ "headers" .= headerInfo req
    , "origin" .= originInfo req
    , "args" .= getInfo req]


headerInfo :: Request -> Value
headerInfo req = Object $ fromList (headers req)
  where
    headers :: Request -> [(T.Text, Value)]
    headers req' =
      map
        (\(k,v) -> (decodeUtf8 (original k), String $ decodeUtf8 v))
        (requestHeaders req')


originInfo :: Request -> Value
originInfo req = trans $ remoteHost req
  where
    trans (SockAddrInet _ hostAddr) = toIp $ hostAddressToTuple hostAddr
    trans _ = "n/a"
    toIp (a,b,c,d) =
      String . T.pack $ intercalate "." [show a, show b, show c, show d]


getInfo :: Request -> Value
getInfo req =
  let tupleToValue (k,v) = decodeUtf8 k .= maybe "" decodeUtf8 v
  in Object . fromList $ map tupleToValue (queryString req)
