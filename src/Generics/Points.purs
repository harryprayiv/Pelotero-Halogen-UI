module Points where

import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Either (Either(..))
import Data.Tuple


data PlayerResults
  = BattingResults (Maybe BattingStats)
  | PitchingResults (Maybe PitchingStats)
  | NoStats

derive instance genericPlayerResults :: Generic PlayerResults _

instance encodeJsonPlayerResults :: EncodeJson PlayerResults where
  encodeJson = genericEncodeJson

instance decodeJsonPlayerResults :: DecodeJson PlayerResults where
  decodeJson = genericDecodeJson


data Results = Results
  { cC  :: Tuple String Number
  , b1C :: Tuple String Number
  , b2C :: Tuple String Number
  , b3C :: Tuple String Number
  , ssC :: Tuple String Number
  , ofC :: Array (Tuple String Number)
  , uC  :: Tuple String Number
  , spC :: Array (Tuple String Number)
  , rpC :: Array (Tuple String Number)
  }

derive instance genericResults :: Generic Results _

instance encodeJsonResults :: EncodeJson Results where
  encodeJson = genericEncodeJson

instance decodeJsonResults :: DecodeJson Results where
  decodeJson = genericDecodeJson


data StatType = Batting | Pitching

derive instance genericStatType :: Generic StatType _

instance encodeJsonStatType :: EncodeJson StatType where
  encodeJson = genericEncodeJson

instance decodeJsonStatType :: DecodeJson StatType where
  decodeJson = genericDecodeJson

