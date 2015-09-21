{-# LANGUAGE ConstraintKinds #-}

module Lib where

-- component
import Cache
import Discourse
import Gitter
-- general
import            Control.Lens
import            Control.Monad.Logger
import            Control.Monad.Reader
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text

data Config = Config { _config_roomUri :: RoomUri }
makeLenses ''Config

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
                      )

repostUpdates :: MonadRepost m => m ()
repostUpdates = do
    latestTopics <- Discourse.getLatest
    cachedTopics <- loadDef []
    let newTopics = detectNewTopics cachedTopics latestTopics
    $logDebug ("newTopics = " <> showText newTopics)
    save latestTopics

    roomUri <- view config_roomUri
    let message = "new topic!"
    withGitter .
        withRoom roomUri $
            SendChatMessage message

showText :: Show a => a -> Text
showText = Text.pack . show
