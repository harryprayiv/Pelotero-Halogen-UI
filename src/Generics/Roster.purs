module Roster where

import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

-- LgManager ADT
data LgManager = LgManager
  { status :: String
  , commissioner :: String
  , teamId :: String
  , leagueID :: String
  , currentLineup :: CurrentLineup
  , roster :: Roster
  }

data CurrentLineup = CurrentLineup
  { cC :: String
  , b1C :: String
  , b2C :: String
  , b3C :: String
  , ssC :: String
  , ofC :: Array String
  , uC :: String
  , spC :: Array String
  , rpC :: Array String
  }

data Roster = Roster
  { cR :: Array String
  , b1R :: Array String
  , b2R :: Array String
  , b3R :: Array String
  , ssR :: Array String
  , ofR :: Array String
  , uR :: Array String
  , spR :: Array String
  , rpR :: Array String
  }

derive instance genericLgManager :: Generic LgManager _
derive instance genericCurrentLineup :: Generic CurrentLineup _
derive instance genericRoster :: Generic Roster _

instance encodeJsonLgManager :: EncodeJson LgManager where
  encodeJson = genericEncodeJson

instance decodeJsonLgManager :: DecodeJson LgManager where
  decodeJson = genericDecodeJson

instance encodeJsonCurrentLineup :: EncodeJson CurrentLineup where
  encodeJson = genericEncodeJson

instance decodeJsonCurrentLineup :: DecodeJson CurrentLineup where
  decodeJson = genericDecodeJson

instance encodeJsonRoster :: EncodeJson Roster where
  encodeJson = genericEncodeJson

instance decodeJsonRoster :: DecodeJson Roster where
  decodeJson = genericDecodeJson
