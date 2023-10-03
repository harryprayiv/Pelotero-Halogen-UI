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

data Position = Position
    {pos_code :: Text} 

data BattingStats = BattingStats 
  {  bat_gamesPlayed :: Maybe Int
  , bat_flyOuts :: Maybe Int
  , bat_groundOuts :: Maybe Int
  , bat_runs :: Maybe Int
  , bat_doubles :: Maybe Int
  , bat_triples :: Maybe Int
  , bat_homeRuns :: Maybe Int
  , bat_strikeOuts :: Maybe Int
  , bat_baseOnBalls :: Maybe Int
  , bat_intentionalWalks :: Maybe Int
  , bat_hits :: Maybe Int
  , bat_hitByPitch :: Maybe Int
  , bat_atBats :: Maybe Int
  , bat_caughtStealing :: Maybe Int
  , bat_stolenBases :: Maybe Int
  , bat_groundIntoDoublePlay :: Maybe Int
  , bat_groundIntoTriplePlay :: Maybe Int
  , bat_plateAppearances :: Maybe Int
  , bat_totalBases :: Maybe Int
  , bat_rbi :: Maybe Int
  , bat_leftOnBase :: Maybe Int
  , bat_sacBunts :: Maybe Int
  , bat_sacFlies :: Maybe Int
  , bat_catchersInterference :: Maybe Int
  , bat_pickoffs :: Maybe Int}

data PitchingStats = PitchingStats 
  {  pit_gamesPlayed :: Maybe Int
  ,  pit_gamesStarted :: Maybe Int
  ,  pit_flyOuts :: Maybe Int
  ,  pit_groundOuts :: Maybe Int
  ,  pit_airOuts :: Maybe Int
  ,  pit_runs :: Maybe Int
  ,  pit_doubles :: Maybe Int
  ,  pit_triples :: Maybe Int
  ,  pit_homeRuns :: Maybe Int
  ,  pit_strikeOuts :: Maybe Int
  ,  pit_baseOnBalls :: Maybe Int
  ,  pit_intentionalWalks :: Maybe Int
  ,  pit_hits :: Maybe Int
  ,  pit_hitByPitch :: Maybe Int
  ,  pit_atBats :: Maybe Int
  ,  pit_caughtStealing :: Maybe Int
  ,  pit_stolenBases :: Maybe Int
  ,  pit_numberOfPitches :: Maybe Int
  ,  pit_inningsPitched :: Maybe String
  ,  pit_wins :: Maybe Int
  ,  pit_losses :: Maybe Int
  ,  pit_saves :: Maybe Int
  ,  pit_saveOpportunities :: Maybe Int
  ,  pit_holds :: Maybe Int
  ,  pit_blownSaves :: Maybe Int
  ,  pit_earnedRuns :: Maybe Int
  ,  pit_battersFaced :: Maybe Int
  ,  pit_outs :: Maybe Int
  ,  pit_gamesPitched :: Maybe Int
  ,  pit_completeGames :: Maybe Int
  ,  pit_shutouts :: Maybe Int
  ,  pit_pitchesThrown :: Maybe Int
  ,  pit_balls :: Maybe Int
  ,  pit_strikes :: Maybe Int
  ,  pit_hitBatsmen :: Maybe Int
  ,  pit_balks :: Maybe Int
  ,  pit_wildPitches :: Maybe Int
  ,  pit_pickoffs :: Maybe Int
  ,  pit_rbi :: Maybe Int
  ,  pit_gamesFinished :: Maybe Int
  ,  pit_inheritedRunners :: Maybe Int
  ,  pit_inheritedRunnersScored :: Maybe Int
  ,  pit_catchersInterference :: Maybe Int
  ,  pit_sacBunts :: Maybe Int
  ,  pit_sacFlies :: Maybe Int
  ,  pit_passedBall :: Maybe Int}


-- Deriving generics:
derive instance genericJsonPlayerData :: Generic JsonPlayerData _
derive instance genericJsonStatsData :: Generic JsonStatsData _

-- Encode/Decode instances:
-- instance encodeJsonJsonPlayerData :: EncodeJson JsonPlayerData where
--   encodeJson = genericEncodeJson

-- instance decodeJsonJsonPlayerData :: DecodeJson JsonPlayerData where
--   decodeJson = genericDecodeJson

-- instance encodeJsonJsonStatsData :: EncodeJson JsonStatsData where
--   encodeJson = genericEncodeJson

-- instance decodeJsonJsonStatsData :: DecodeJson JsonStatsData where
--   decodeJson = genericDecodeJson