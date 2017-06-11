module Config where

import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
       (logStdout, logStdoutDev)
import Text.Read (readMaybe)
import System.Environment (lookupEnv)


newtype Config = Config { getEnv :: Environment }


data Environment
  = Development
  | Test
  | Production
  deriving (Eq, Show, Read)


setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout


lookupSettingSafe
  :: Read a
  => String -> a -> IO a
lookupSettingSafe env def = do
  maybeValue <- lookupEnv env
  case maybeValue of
    Nothing -> return def
    Just str -> maybe (handleFailedRead str) return (readMaybe str)
  where
    handleFailedRead str =
      error $
      mconcat ["Failed to read [[", str, "]] for environment variable ", env]
