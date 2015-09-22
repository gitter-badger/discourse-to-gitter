{-# LANGUAGE  FlexibleInstances
            , GeneralizedNewtypeDeriving
            , MultiParamTypeClasses
  #-}

module TestIO (Effect(..), TestIO, TestIOResult(..), execTestIO) where

-- package
import Cache
import Config
import Discourse
import Gitter
import HttpClient
-- general
import Control.Monad.Logger
import Control.Monad.RWS
import Data.Aeson
import Data.ByteString.Lazy as ByteString

data Effect = CacheRead | CacheWrite | DiscourseGet String | HttpRequest String ByteString
    deriving (Eq, Show)

newtype TestIO a = TestIO (RWST Config [Effect] [Topic] (LoggingT IO) a)
    deriving (Applicative, Functor, Monad, MonadLogger, MonadReader Config)

instance MonadCache [Topic] TestIO where
    loadDef def = TestIO $ do
        tell [CacheRead]
        loadDef def
    save val = TestIO $ do
        tell [CacheWrite]
        save val

instance MonadDiscourse TestIO where
    getLatest = TestIO $ do
        tell [DiscourseGet "/latest.json"]
        jsonContent <- liftIO (decodeFile "test/data/discourse/latest.json")
        fromRight (decodeLatestResponse jsonContent)
      where
        fromRight = either fail return

instance MonadHttpClient TestIO where
    runHttpClient url body = do
        TestIO $ tell [HttpRequest url body]
        return (mockGitter url body)

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile filepath = do
    bytes <- ByteString.readFile filepath
    let decodeResult = eitherDecode bytes
    case decodeResult of
        Left decodeError ->
            error ("Cannot decode file \"" <> filepath <> "\": " <> decodeError)
        Right value ->
            return value

data TestIOResult = TestIOResult  { testIOResult_effects  :: [Effect]
                                  , testIOResult_cache    :: [Topic]
                                  }

execTestIO :: TestIO () -> IO TestIOResult
execTestIO testAction = do
    let cache = []
        TestIO rwsAction = testAction
        loggingAction = execRWST rwsAction testConfig cache
        ioAction = runStderrLoggingT loggingAction
    (testIOResult_cache, testIOResult_effects) <- ioAction
    return TestIOResult{..}

testConfig :: Config
testConfig =
    Config  { _config_gitterBaseUrl = "test://api.gitter.example.com/v1"
            , _config_room = RoomOneToOne "cblp"
            }

mockGitter :: String -> ByteString -> ByteString
mockGitter url req =
    let err = error ("don't know how to mock " <> show url <> ":" <> show req)
    in case url of
        "test://api.gitter.example.com/v1/rooms" -> case req of
            "{\"uri\":\"cblp\"}" -> "{\"id\":\"exampleroomid\"}"
            _ -> err
        "test://api.gitter.example.com/v1/room/exampleroomid/chatMessages" ->
            case req of
                "{\"text\":\"new topic!\"}" -> "{}"
                _ -> err
        _ -> err
