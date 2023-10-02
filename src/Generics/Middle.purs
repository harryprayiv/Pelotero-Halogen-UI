module Middle where

-- import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)


-- import Data.StrMap (StrMap)
import Data.Array 

type Text = String

data JsonPlayerData = JsonPlayerData
  { playerId :: Text
  , fullName :: Text
  , stats    :: Array JsonStatsData  -- not even a thing anymore
  }

data JsonStatsData = JsonStatsData
  { parentTeamId    :: Int
  , allPositions    :: Array Position
  , statusCode      :: Text
  , batting         :: Maybe BattingStats
  , pitching        :: Maybe PitchingStats
  }


-- Deriving generics:
derive instance genericJsonPlayerData :: Generic JsonPlayerData _
derive instance genericJsonStatsData :: Generic JsonStatsData _

-- Encode/Decode instances:
instance encodeJsonJsonPlayerData :: EncodeJson JsonPlayerData where
  encodeJson = genericEncodeJson

instance decodeJsonJsonPlayerData :: DecodeJson JsonPlayerData where
  decodeJson = genericDecodeJson

instance encodeJsonJsonStatsData :: EncodeJson JsonStatsData where
  encodeJson = genericEncodeJson

instance decodeJsonJsonStatsData :: DecodeJson JsonStatsData where
  decodeJson = genericDecodeJson