-- module Roster where

-- import Prelude
-- -- import Data.Argonaut.Decode (class DecodeJson, jsonParser)
-- -- import Data.Argonaut.Encode (class EncodeJson, encodeJson)
-- -- import Data.Argonaut.Core (Json, jsonEmptyObject, (:=))
-- -- import Data.Argonaut.Parser (jsonParser)
-- import Data.Either (Either)

-- type Player = 
--   { playerId :: Int
--   , nameSlug :: String
--   , useName :: String
--   , useLastName :: String
--   , currentTeam :: Int
--   , primaryPosition :: String
--   , active :: Boolean
--   , batSide :: String
--   , pitchHand :: String
--   }


-- instance encodeJsonPlayer :: EncodeJson Player where
--   encodeJson player = 
--     jsonEmptyObject
--       := "playerId" := player.playerId
--       := "nameSlug" := player.nameSlug
--       -- ... other fields ...

-- type Players = Record (StrMap Player)

-- type Roster = 
--   { checksum :: String
--   , dataPulled :: String
--   , officialPlayers :: Players
--   }

-- instance encodeJsonRoster :: EncodeJson Roster where
--   encodeJson roster = 
--     jsonEmptyObject
--       := "checksum" := roster.checksum
--       := "dataPulled" := roster.dataPulled
--       := "officialPlayers" := encodeJson roster.officialPlayers

-- -- instance decodeJsonPlayer :: DecodeJson Player where
-- --   decodeJson = jsonParser { playerId: "playerId", nameSlug: "nameSlug", -- ... other fields ... }

-- -- instance decodeJsonRoster :: DecodeJson Roster where
-- --   decodeJson = jsonParser { checksum: "checksum", dataPulled: "dataPulled", officialPlayers: "officialPlayers" }

