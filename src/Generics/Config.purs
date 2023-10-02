module Config where

-- import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Either (Either(..))

-- Assuming the use of the `purescript-strings` library for Text type.
-- import Data.String (String)

data Configuration = Configuration
  { status :: String
  , leagueID :: String
  , point_parameters :: PointParameters
  , draft_parameters :: DraftParameters
  , commissioner :: String
  , lgMembers :: Array String
  }

data PointParameters = PointParameters
  { lg_style :: String
  , start_UTC :: String
  , end_UTC :: String
  , lg_battingMults :: BattingMults
  , lg_pitchingMults :: PitchingMults
  , valid_roster :: LgRoster
  }

data BattingMults = BattingMults
  { lgb_single :: Number
  , lgb_double :: Number
  , lgb_triple :: Number
  , lgb_homerun :: Number
  , lgb_rbi :: Number
  , lgb_run :: Number
  , lgb_base_on_balls :: Number
  , lgb_stolen_base :: Number
  , lgb_hit_by_pitch :: Number
  , lgb_strikeout :: Number
  , lgb_caught_stealing :: Number
  }

data PitchingMults = PitchingMults
  { lgp_win :: Number
  , lgp_save :: Number
  , lgp_quality_start :: Number
  , lgp_inning_pitched :: Number
  , lgp_strikeout :: Number
  , lgp_complete_game :: Number
  , lgp_shutout :: Number
  , lgp_base_on_balls :: Number
  , lgp_hits_allowed :: Number
  , lgp_earned_runs :: Number
  , lgp_hit_batsman :: Number
  , lgp_loss :: Number
  }

data LgRoster = LgRoster
  { lg_catcher :: Int
  , lg_first :: Int
  , lg_second :: Int
  , lg_third :: Int
  , lg_shortstop :: Int
  , lg_outfield :: Int
  , lg_utility :: Int
  , lg_s_pitcher :: Int
  , lg_r_pitcher :: Int
  , lg_max_size :: Int
  }

data DraftRoster = DraftRoster
  { dr_catcher :: Int
  , dr_first :: Int
  , dr_second :: Int
  , dr_third :: Int
  , dr_shortstop :: Int
  , dr_outfield :: Int
  , dr_utility :: Int
  , dr_s_pitcher :: Int
  , dr_r_pitcher :: Int
  }

data DraftParameters = DraftParameters
  { autoDraft :: Boolean
  , autoDraft_UTC :: String
  , draft_limits :: DraftRoster
  }

-- Deriving Generic instances
derive instance genericConfiguration :: Generic Configuration _
derive instance genericPointParameters :: Generic PointParameters _
derive instance genericBattingMults :: Generic BattingMults _
derive instance genericPitchingMults :: Generic PitchingMults _
derive instance genericLgRoster :: Generic LgRoster _
derive instance genericDraftRoster :: Generic DraftRoster _
derive instance genericDraftParameters :: Generic DraftParameters _

-- JSON instances using Generics
instance encodeJsonConfiguration :: EncodeJson Configuration where
  encodeJson = genericEncodeJson

instance decodeJsonConfiguration :: DecodeJson Configuration where
  decodeJson = genericDecodeJson