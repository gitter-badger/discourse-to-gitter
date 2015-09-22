{-# LANGUAGE NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Gitter
    ( Gitter(..)
    , Room(..)
    , withGitter
    , withRoom
    , sendChatMessage
    ) where

-- component
import HttpClient
-- global
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid
import Data.Text
import Network.HTTP.Client

type ResourcePath = [Text]
type UserName = Text
type RepoName = Text
type RoomId = Text
type RoomUri = Text
data Room = RoomOneToOne UserName | RoomRepo UserName RepoName

data Gitter = Gitter { gitter_baseUrl :: Request }

deriveFromJSON defaultOptions ''Room

newtype GitterT m a = GitterT (ReaderT Gitter m a)
    deriving (Applicative, Functor, Monad)

newtype GitterRoomT m a = GitterRoomT (ReaderT Room (GitterT m) a)

withGitter :: Monad m => Gitter -> GitterT m a -> m a
withGitter gitter (GitterT readerAction) =
    runReaderT readerAction gitter

withRoom :: Room -> GitterRoomT m a -> GitterT m a
withRoom room (GitterRoomT readerAction) =
    runReaderT readerAction room

sendChatMessage :: MonadHttpClient m => Text -> GitterRoomT m a
sendChatMessage text =
    runRoomAction ["chatMessages"] $ object [("text", String text)]

runRoomAction :: MonadHttpClient m => ResourcePath -> Value -> GitterRoomT m a
runRoomAction path request = GitterRoomT $ do
    room <- ask
    roomId <- lift $ joinRoom room
    lift $ runGitterAction (["room", roomId] <> path) request

joinRoom :: MonadHttpClient m => Room -> GitterT m RoomId
joinRoom room =
    runGitterAction ["rooms"] $ object [("uri", String $ roomUri room)]

runGitterAction :: MonadHttpClient m => ResourcePath -> Value -> GitterT m Value
runGitterAction path apiRequest = GitterT $ do
    Gitter { gitter_baseUrl } <- ask
    let httpRequest = gitter_baseUrl
    rawResponse <- lift $ runHttpClient httpRequest
    either fail return $ eitherDecode rawResponse

roomUri :: Room -> RoomUri
roomUri (RoomOneToOne user) = user
roomUri (RoomRepo user repo) = user <> "/" <> repo
