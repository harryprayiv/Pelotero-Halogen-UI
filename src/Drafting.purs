module Drafting (WidgetNode, render, form) where

import Prelude (($), (>>>))
import Data.Array (foldl)
import Data.Maybe (Maybe (Just, Nothing), maybe)
import Data.Semigroup ((<>))

import Node.FS.Aff as FS
import Data.Argonaut (Json, parseJson)
import Effect.Aff (Aff)
-- import Roster

type InputType = String
type Label = String
type Method = String
type Action = String
type Name = String
type Value = String
type HTML = String

type RosterRow = { tag1 :: String, tag2 :: Int, tag3 :: Boolean }

data Widget = Input InputType (Maybe Name) Label | Form Method Action
data WidgetNode = WidgetNode Widget (Array WidgetNode)
  
submit :: Widget
submit = Input "submit" Nothing " Submit"

address :: Widget
address = Input "text" (Just "wallet") "Drafting"

formWidget :: Widget
formWidget = Form "post" "https://httpbin.org/post"


data HTMLAttr = Attr Name Value

foldAcc :: forall a. (a -> String) -> String -> a -> String
foldAcc fn s = fn >>> (<>) s

renderAttr :: HTMLAttr -> HTML
renderAttr (Attr name value) = " " <> name <> "=\"" <> value <> "\""

tag :: HTML -> Name -> Array HTMLAttr -> HTML
tag content name attrs = let attrs' = foldl (foldAcc renderAttr) "" attrs in
  "<" <> name <> attrs' <> ">" <> content <> "</" <> name <> ">"

renderElement :: Widget -> String -> String
renderElement el ct = let tag' = tag ct in case el of
  (Form mt act) -> tag' "form" $ [Attr "method" mt, Attr "action" act]
  (Input t n "") -> tag' "input" $ [Attr "type" t] <> maybe [] (\n' -> [Attr "name" n']) n
  (Input t n lb) -> tag ( lb <> renderElement (Input t n "") "" ) "label" []

render :: WidgetNode -> String
render (WidgetNode el children) = let ct = foldl (foldAcc render) "" children in
  renderElement el ct

form :: WidgetNode
form = WidgetNode formWidget [WidgetNode address [], WidgetNode submit []]

-- loadAndProcessRoster: function takes the file path of the JSON file as an argument.
-- FS.readTextFile FS.utf8 filePath reads the file contents.
--parseJson fileContent attempts to parse the file content into a Json object.
--decodeJson json :: Maybe (Array RosterRow) attempts to decode the Json object into an array of RosterRow objects.
loadAndProcessRoster :: String -> Aff Unit
loadAndProcessRoster filePath = do
  fileContent <- FS.readTextFile FS.utf8 filePath
  case jsonParser fileContent of
    Right json -> case decodeJson json of
      Right roster -> processRoster roster
      Left err -> logShow $ "Error decoding JSON: " <> err
    Left err -> logShow $ "Error parsing JSON: " <> err

--processRoster rows is a separate function where you would process the roster data as needed.
-- processRoster :: Roster -> Aff Unit
-- processRoster roster = do
--   -- process the roster data ...
--   pure unit
