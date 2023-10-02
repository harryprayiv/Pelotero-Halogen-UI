module OfficialRoster where

-- import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

data Roster = Roster
  { checksum :: String
  , dataPulled :: String
  , officialPlayers :: Array Player -- TODO: this was StrMap Player but that is deprecated now!! 
  }

data Player = Player
  { playerId :: Int
  , useName :: Maybe String
  , useLastName :: Maybe String
  , nameSlug :: Maybe String
  , currentTeam :: Maybe Int
  , primaryPosition :: Maybe String
  , batSide :: Maybe String
  , pitchHand :: Maybe String
  , active :: Boolean
  }

derive instance genericRoster :: Generic Roster _
derive instance genericPlayer :: Generic Player _

instance encodeJsonRoster :: EncodeJson Roster where
  encodeJson = genericEncodeJson

instance decodeJsonRoster :: DecodeJson Roster where
  decodeJson = genericDecodeJson

instance encodeJsonPlayer :: EncodeJson Player where
  encodeJson = genericEncodeJson

instance decodeJsonPlayer :: DecodeJson Player where
  decodeJson = genericDecodeJson