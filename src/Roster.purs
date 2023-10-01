module Roster where

import Prelude
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Core (Json, jsonEmptyObject)
import Data.Codec.Argonaut.Record (object)
import Record.Builder
import Data.Argonaut.Encode.Combinators ((:=))
import Data.Argonaut.Parser (jsonParser)
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Map (toUnfoldable) as Map
import Data.Map
import Data.Tuple

import Data.Argonaut (class EncodeJson, encodeJson, Json, (:=))
import Data.Argonaut.Encode.Combinators as EncodeJson
import Data.Map as Map
import Data.Foldable (foldl)

data Player = Player
  { playerId :: Int
  , nameSlug :: Maybe String
  , useName :: Maybe String
  , useLastName :: Maybe String
  , currentTeam :: Maybe Int
  , primaryPosition :: Maybe String
  , active :: Boolean
  , batSide :: Maybe String
  , pitchHand :: Maybe String
  }

data Roster = Roster
  { checksum :: Maybe String
  , dataPulled :: Maybe String
  , officialPlayers :: Map String Player
  }

instance encodeJsonPlayer :: EncodeJson Player where
  encodeJson (Player player) = 
    EncodeJson.object
      [ "playerId" := EncodeJson.encodeJson player.playerId
      , "nameSlug" := EncodeJson.encodeJson player.nameSlug
      , "useName" := EncodeJson.encodeJson player.useName
      , "useLastName" := EncodeJson.encodeJson player.useLastName
      , "currentTeam" := EncodeJson.encodeJson player.currentTeam
      , "primaryPosition" := EncodeJson.encodeJson player.primaryPosition
      , "active" := EncodeJson.encodeJson player.active
      , "batSide" := EncodeJson.encodeJson player.batSide
      , "pitchHand" := EncodeJson.encodeJson player.pitchHand
      ]

mapToJson :: forall a. EncodeJson a => Map.Map String a -> Json
mapToJson m = 
  let
    toJsonArr (Tuple k v) = [k, EncodeJson.encodeJson v]
    kvPairs = Map.toUnfoldable m
  in
    EncodeJson.object (Array.concatMap toJsonArr kvPairs)

instance encodeJsonRoster :: EncodeJson Roster where
  encodeJson roster = 
    EncodeJson.object
      [ "checksum" := EncodeJson.encodeJson roster.checksum
      , "dataPulled" := EncodeJson.encodeJson roster.dataPulled
      , "officialPlayers" := mapToJson roster.officialPlayers
      ]

instance decodeJsonPlayer :: DecodeJson Player where
  decodeJson = jsonParser
    { playerId: "playerId"
    , nameSlug: "nameSlug"
    , useName: "useName"
    , useLastName: "useLastName"
    , currentTeam: "currentTeam"
    , primaryPosition: "primaryPosition"
    , active: "active"
    , batSide: "batSide"
    , pitchHand: "pitchHand"
    }

instance decodeJsonRoster :: DecodeJson Roster where
  decodeJson = jsonParser
    { checksum: "checksum"
    , dataPulled: "dataPulled"
    , officialPlayers: "officialPlayers"
    }