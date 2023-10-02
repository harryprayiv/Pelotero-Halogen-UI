module Input where

-- import Prelude
-- import Data.Argonaut.Decode.Class (class DecodeJson)
-- import Data.Argonaut.Decode.Generic (genericDecodeJson)
-- import Data.Argonaut.Encode.Class (class EncodeJson)
-- import Data.Argonaut.Encode.Generic (genericEncodeJson)
-- import Data.Generic.Rep (class Generic)
-- import Data.Maybe (Maybe)

-- -- ## Boxscore Stats data type
-- data GameData where
--   GameData :: {teams :: Teams} -> GameData
--   deriving (Show, Eq)

-- data Teams where
--   Teams :: {away :: TeamData, home :: TeamData} -> Teams
--   deriving (Show, Eq)

-- data TeamData where
--   TeamData :: {players :: M.Map Text Player} -> TeamData
--   deriving (Show, Eq)

-- type Players = [(Text, Player)]

-- data Player where
--   Player :: {person :: Person,
--                gameid :: Maybe Int,
--                parentTeamId :: Int,
--                allPositions :: Maybe [Position],
--                status :: Status,
--                stats :: PlayerStats}
--               -> Player
--   deriving (Show, Eq)

-- data Person where
--   Person :: {personId :: Int, fullName :: Text} -> Person
--   deriving (Show, Eq)

-- data Position where
--   Position :: {pos_code :: Text} -> Position
--   deriving (Show, Eq)

-- data Status where
--   Status :: {status_code :: Text} -> Status
--   deriving (Show, Eq)

-- data PlayerStats where
--   PlayerStats :: {  
--                     batting :: Maybe BattingStats,
--                     pitching :: Maybe PitchingStats}
--                    -> PlayerStats
--   deriving (Show, Eq)

-- data BattingStats where
--   BattingStats :: {  bat_gamesPlayed :: Maybe Int,
--                      bat_flyOuts :: Maybe Int,
--                      bat_groundOuts :: Maybe Int,
--                      bat_runs :: Maybe Int,
--                      bat_doubles :: Maybe Int,
--                      bat_triples :: Maybe Int,
--                      bat_homeRuns :: Maybe Int,
--                      bat_strikeOuts :: Maybe Int,
--                      bat_baseOnBalls :: Maybe Int,
--                      bat_intentionalWalks :: Maybe Int,
--                      bat_hits :: Maybe Int,
--                      bat_hitByPitch :: Maybe Int,
--                      bat_atBats :: Maybe Int,
--                      bat_caughtStealing :: Maybe Int,
--                      bat_stolenBases :: Maybe Int,
--                      bat_groundIntoDoublePlay :: Maybe Int,
--                      bat_groundIntoTriplePlay :: Maybe Int,
--                      bat_plateAppearances :: Maybe Int,
--                      bat_totalBases :: Maybe Int,
--                      bat_rbi :: Maybe Int,
--                      bat_leftOnBase :: Maybe Int,
--                      bat_sacBunts :: Maybe Int,
--                      bat_sacFlies :: Maybe Int,
--                      bat_catchersInterference :: Maybe Int,
--                      bat_pickoffs :: Maybe Int
--                      }
--                     -> BattingStats
--   deriving (Show, Eq)

-- data PitchingStats where
--   PitchingStats :: {  pit_gamesPlayed :: Maybe Int,
--                       pit_gamesStarted :: Maybe Int,
--                       pit_flyOuts :: Maybe Int,
--                       pit_groundOuts :: Maybe Int,
--                       pit_airOuts :: Maybe Int,
--                       pit_runs :: Maybe Int,
--                       pit_doubles :: Maybe Int,
--                       pit_triples :: Maybe Int,
--                       pit_homeRuns :: Maybe Int,
--                       pit_strikeOuts :: Maybe Int,
--                       pit_baseOnBalls :: Maybe Int,
--                       pit_intentionalWalks :: Maybe Int,
--                       pit_hits :: Maybe Int,
--                       pit_hitByPitch :: Maybe Int,
--                       pit_atBats :: Maybe Int,
--                       pit_caughtStealing :: Maybe Int,
--                       pit_stolenBases :: Maybe Int,
--                       pit_numberOfPitches :: Maybe Int,
--                       pit_inningsPitched :: Maybe Text,
--                       pit_wins :: Maybe Int,
--                       pit_losses :: Maybe Int,
--                       pit_saves :: Maybe Int,
--                       pit_saveOpportunities :: Maybe Int,
--                       pit_holds :: Maybe Int,
--                       pit_blownSaves :: Maybe Int,
--                       pit_earnedRuns :: Maybe Int,
--                       pit_battersFaced :: Maybe Int,
--                       pit_outs :: Maybe Int,
--                       pit_gamesPitched :: Maybe Int,
--                       pit_completeGames :: Maybe Int,
--                       pit_shutouts :: Maybe Int,
--                       pit_pitchesThrown :: Maybe Int,
--                       pit_balls :: Maybe Int,
--                       pit_strikes :: Maybe Int,
--                       pit_hitBatsmen :: Maybe Int,
--                       pit_balks :: Maybe Int,
--                       pit_wildPitches :: Maybe Int,
--                       pit_pickoffs :: Maybe Int,
--                       pit_rbi :: Maybe Int,
--                       pit_gamesFinished :: Maybe Int,
--                       pit_inheritedRunners :: Maybe Int,
--                       pit_inheritedRunnersScored :: Maybe Int,
--                       pit_catchersInterference :: Maybe Int,
--                       pit_sacBunts :: Maybe Int,
--                       pit_sacFlies :: Maybe Int,
--                       pit_passedBall :: Maybe Int
--                       }
--                      -> PitchingStats
--   deriving (Show, Eq)

-- -- ## Schedule ADT's ##
-- data GameSchedule where
--   GameSchedule :: {dates :: [DateEntry]} -> GameSchedule
--   deriving (Show, Eq)

-- data DateEntry where
--   DateEntry :: {games :: Maybe (V.Vector GameID)} -> DateEntry
--   deriving (Show, Eq)

-- data GameID where
--   GameID :: {
--               gamePk :: Int,
--               game_date :: Maybe Text
--             } -> GameID
--   deriving (Show, Eq)

-- -- Top level structure for the active roster
-- data ActiveRoster where
--   ActiveRoster :: {
--                     people :: [ActivePlayer],
--                     dataPulled :: Maybe Text,
--                     checksum :: Maybe Text 
--                   } -> ActiveRoster

-- data ActivePlayer where
--   ActivePlayer :: {  playerId :: Int,
--                      useName :: Maybe Text,
--                      useLastName :: Maybe Text,
--                      nameSlug :: Maybe Text,
--                      currentTeam :: Maybe Int,
--                      primaryPosition :: Maybe Text,
--                      batSide :: Maybe Text,
--                      pitchHand :: Maybe Text,
--                      active :: Bool}
--                     -> ActivePlayer
--   deriving (Show, Eq)
  
-- -- ## Game Status ADT's ##
-- data LiveGameStatus where
--   LiveGameStatus :: {codedGameState :: Text} -> LiveGameStatus
--   deriving (Show, Eq)

-- data LiveGameStatusWrapper where
--   LiveGameStatusWrapper :: {gameStatus :: LiveGameStatus} -> LiveGameStatusWrapper
--   deriving (Show, Eq)

-- data LiveGameWrapper where
--   LiveGameWrapper :: {gameData :: LiveGameStatusWrapper} -> LiveGameWrapper
--   deriving (Show, Eq)