module Leaderboard where

-- import Prelude
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)

import Data.Maybe (Maybe)
-- import Data.StrMap (StrMap)
-- import Data.Text (Text) -- Assuming Text maps to String in Purescript

data SeasonData = SeasonData { teams :: Teams }

derive instance genericSeasonData :: Generic SeasonData _

instance encodeJsonSeasonData :: EncodeJson SeasonData where
  encodeJson = genericEncodeJson

instance decodeJsonSeasonData :: DecodeJson SeasonData where
  decodeJson = genericDecodeJson

data Teams = Teams { away :: TeamData, home :: TeamData }

derive instance genericTeams :: Generic Teams _

instance encodeJsonTeams :: EncodeJson Teams where
  encodeJson = genericEncodeJson

instance decodeJsonTeams :: DecodeJson Teams where
  decodeJson = genericDecodeJson

data TeamData = TeamData { players :: Array Player }

derive instance genericTeamData :: Generic TeamData _

instance encodeJsonTeamData :: EncodeJson TeamData where
  encodeJson = genericEncodeJson

instance decodeJsonTeamData :: DecodeJson TeamData where
  decodeJson = genericDecodeJson

data Player = Player
  { person :: Person
  , status :: Status
  , seasonStats :: PlayerSeasonStats
  }


data BattingTotals = BattingTotals
  { bat_T_gamesPlayed :: Maybe Int
  , bat_T_flyOuts :: Maybe Int
  , bat_T_groundOuts :: Maybe Int
  , bat_T_runs :: Maybe Int
  , bat_T_doubles :: Maybe Int
  , bat_T_triples :: Maybe Int
  , bat_T_homeRuns :: Maybe Int
  , bat_T_strikeOuts :: Maybe Int
  , bat_T_baseOnBalls :: Maybe Int
  , bat_T_intentionalWalks :: Maybe Int
  , bat_T_hits :: Maybe Int
  , bat_T_hitByPitch :: Maybe Int
  , bat_T_avg :: Maybe String
  , bat_T_atBats :: Maybe Int
  , bat_T_obp :: Maybe String
  , bat_T_slg :: Maybe String
  , bat_T_ops :: Maybe String
  , bat_T_caughtStealing :: Maybe Int
  , bat_T_stolenBases :: Maybe Int
  , bat_T_stolenBasePercentage :: Maybe String
  , bat_T_groundIntoDoublePlay :: Maybe Int
  , bat_T_groundIntoTriplePlay :: Maybe Int
  , bat_T_plateAppearances :: Maybe Int
  , bat_T_totalBases :: Maybe Int
  , bat_T_rbi :: Maybe Int
  , bat_T_leftOnBase :: Maybe Int
  , bat_T_sacBunts :: Maybe Int
  , bat_T_sacFlies :: Maybe Int
  , bat_T_babip :: Maybe String
  , bat_T_catchersInterference :: Maybe Int
  , bat_T_pickoffs :: Maybe Int
  , bat_T_atBatsPerHomeRun :: Maybe String
  }

derive instance genericBattingTotals :: Generic BattingTotals _

instance encodeJsonBattingTotals :: EncodeJson BattingTotals where
  encodeJson = genericEncodeJson

instance decodeJsonBattingTotals :: DecodeJson BattingTotals where
  decodeJson = genericDecodeJson

data PitchingTotals = PitchingTotals
  { pit_T_gamesPlayed :: Maybe Int
  , pit_T_gamesStarted :: Maybe Int
  , pit_T_groundOuts :: Maybe Int
  , pit_T_airOuts :: Maybe Int
  , pit_T_runs :: Maybe Int
  , pit_T_doubles :: Maybe Int
  , pit_T_triples :: Maybe Int
  , pit_T_homeRuns :: Maybe Int
  , pit_T_strikeOuts :: Maybe Int
  , pit_T_baseOnBalls :: Maybe Int
  , pit_T_intentionalWalks :: Maybe Int
  , pit_T_hits :: Maybe Int
  , pit_T_hitByPitch :: Maybe Int
  , pit_T_atBats :: Maybe Int
  , pit_T_obp :: Maybe String
  , pit_T_caughtStealing :: Maybe Int
  , pit_T_stolenBases :: Maybe Int
  , pit_T_stolenBasePercentage :: Maybe String
  , pit_T_numberOfPitches :: Maybe Int
  , pit_T_era :: Maybe String
  , pit_T_inningsPitched :: Maybe String
  , pit_T_wins :: Maybe Int
  , pit_T_losses :: Maybe Int
  , pit_T_saves :: Maybe Int
  , pit_T_saveOpportunities :: Maybe Int
  , pit_T_holds :: Maybe Int
  , pit_T_blownSaves :: Maybe Int
  , pit_T_earnedRuns :: Maybe Int
  , pit_T_whip :: Maybe String
  , pit_T_battersFaced :: Maybe Int
  , pit_T_outs :: Maybe Int
  , pit_T_gamesPitched :: Maybe Int
  , pit_T_completeGames :: Maybe Int
  , pit_T_shutouts :: Maybe Int
  , pit_T_pitchesThrown :: Maybe Int
  , pit_T_balls :: Maybe Int
  , pit_T_strikes :: Maybe Int
  , pit_T_strikePercentage :: Maybe String
  , pit_T_hitBatsmen :: Maybe Int
  , pit_T_balks :: Maybe Int
  , pit_T_wildPitches :: Maybe Int
  , pit_T_pickoffs :: Maybe Int
  , pit_T_groundOutsToAirouts :: Maybe String
  , pit_T_rbi :: Maybe Int
  , pit_T_winPercentage :: Maybe String
  , pit_T_pitchesPerInning :: Maybe String
  , pit_T_gamesFinished :: Maybe Int
  , pit_T_strikeoutWalkRatio :: Maybe String
  , pit_T_strikeoutsPer9Inn :: Maybe String
  , pit_T_walksPer9Inn :: Maybe String
  , pit_T_hitsPer9Inn :: Maybe String
  , pit_T_runsScoredPer9 :: Maybe String
  , pit_T_homeRunsPer9 :: Maybe String
  , pit_T_inheritedRunners :: Maybe Int
  , pit_T_inheritedRunnersScored :: Maybe Int
  , pit_T_catchersInterference :: Maybe Int
  , pit_T_sacBunts :: Maybe Int
  , pit_T_sacFlies :: Maybe Int
  , pit_T_passedBall :: Maybe Int
  }

derive instance genericPitchingTotals :: Generic PitchingTotals _

instance encodeJsonPitchingTotals :: EncodeJson PitchingTotals where
  encodeJson = genericEncodeJson

instance decodeJsonPitchingTotals :: DecodeJson PitchingTotals where
  decodeJson = genericDecodeJson
