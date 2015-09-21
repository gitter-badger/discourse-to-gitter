module Gitter where

import Data.Aeson
import Data.Text

type Resource = [Text]
type UserName = Text
type RepoName = Text
type RoomId = Text
data RoomUri = RoomOneToOne UserName | RoomRepo UserName RepoName
data GitterAction = GitterPost Resource Value
data RoomAction = SendChatMessage Text

withGitter :: GitterAction -> gitter ()
withGitter = undefined

withRoom :: RoomUri -> RoomAction -> GitterAction
withRoom = undefined

-- sendChatMessage :: RoomId -> Text -> GitterAction
-- sendChatMessage roomId text =
--     GitterPost ["rooms", roomId, "chatMessages"] $
--         object [("text", String text)]
