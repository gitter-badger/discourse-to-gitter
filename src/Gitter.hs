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
import            Control.Lens
import            Control.Monad.Reader
import            Data.Aeson as Json
import            Data.Aeson.Lens
import            Data.Aeson.TH
import            Data.List
import            Data.Monoid
import            Data.Text ( Text )
import qualified  Data.Text as Text

type ResourcePath = [Text]
type UserName = Text
type RepoName = Text
type RoomId = Text
type RoomUri = Text
data Room = RoomOneToOne UserName | RoomRepo UserName RepoName

data Gitter = Gitter { gitter_baseUrl :: String }

deriveFromJSON defaultOptions ''Room

newtype GitterT m a = GitterT (ReaderT Gitter m a)
    deriving (Applicative, Functor, Monad)

newtype GitterRoomT m a = GitterRoomT (ReaderT Room (GitterT m) a)
    deriving Functor

withGitter :: Monad m => Gitter -> GitterT m a -> m a
withGitter gitter (GitterT readerAction) =
    runReaderT readerAction gitter

withRoom :: Room -> GitterRoomT m a -> GitterT m a
withRoom room (GitterRoomT readerAction) =
    runReaderT readerAction room

sendChatMessage :: MonadHttpClient m => Text -> GitterRoomT m ()
sendChatMessage text =
    void . runRoomAction ["chatMessages"] $ object [("text", String text)]

runRoomAction ::
    MonadHttpClient m => ResourcePath -> Value -> GitterRoomT m Value
runRoomAction path request = GitterRoomT $ do
    room <- ask
    roomId <- lift $ joinRoom room
    lift $ runGitterAction (["room", roomId] <> path) request

joinRoom :: MonadHttpClient m => Room -> GitterT m RoomId
joinRoom room = do
    jsonResponse <- runGitterAction ["rooms"] $
        object [("uri", String $ roomUri room)]
    maybe (fail "joining room must return a string \"id\"") return $
        jsonResponse ^? key "id" . _String

runGitterAction :: MonadHttpClient m => ResourcePath -> Value -> GitterT m Value
runGitterAction path apiRequest = GitterT $ do
    Gitter { gitter_baseUrl } <- ask
    let url = intercalate "/" (gitter_baseUrl : fmap Text.unpack path)
    rawResponse <- lift $ runHttpClient url (Json.encode apiRequest)
    either fail return $ eitherDecode rawResponse

roomUri :: Room -> RoomUri
roomUri (RoomOneToOne user) = user
roomUri (RoomRepo user repo) = user <> "/" <> repo
