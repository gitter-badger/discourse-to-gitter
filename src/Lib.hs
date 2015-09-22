{-# LANGUAGE ConstraintKinds, RecordWildCards #-}

module Lib where

-- component
import Cache
import Config
import Discourse
import Gitter
import HttpClient
-- general
import            Control.Lens
import            Control.Monad.Logger
import            Control.Monad.Reader
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text

detectNewTopics :: [Topic] -> [Topic] -> [Topic]
detectNewTopics []   =
    return . maximum
detectNewTopics olds =
    filter $ \topic ->
        any (\old -> topic_id old /= topic_id topic && old <= topic) olds

type MonadRepost m =  ( MonadReader Config m
                      , MonadCache [Topic] m
                      , MonadDiscourse m
                      , MonadLogger m
                      , MonadHttpClient m
                      )

repostUpdates :: MonadRepost m => m ()
repostUpdates = do
    latestTopics <- Discourse.getLatest
    cachedTopics <- loadDef []
    let newTopics = detectNewTopics cachedTopics latestTopics
    $logDebug ("newTopics = " <> showText newTopics)
    save latestTopics

    room <- view config_room
    gitter_baseUrl <- view config_gitterBaseUrl
    let gitter = Gitter { .. }
    let message = "new topic!"
    withGitter gitter .
        withRoom room $
            sendChatMessage message

showText :: Show a => a -> Text
showText = Text.pack . show
