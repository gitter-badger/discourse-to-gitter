module Main where

-- component
import Cache
import Config
import Lib
-- general
import Control.Monad.Logger
import Control.Monad.Reader
import Data.Aeson
import Data.ByteString.Lazy as ByteString

main :: IO ()
main =  run repostUpdates
            "/etc/discourse-to-gitter.cfg"
            "/var/cache/discourse-to-gitter/latest"
  where
    run action configFile cacheFile = do
        config <- loadConfig configFile
        runStderrLoggingT (runReaderT (runFileCacheT action cacheFile) config)

loadConfig :: FilePath -> IO Config
loadConfig filePath = do
    contents <- ByteString.readFile filePath
    either fail return $ eitherDecode contents
