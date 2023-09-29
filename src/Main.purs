module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Player = 
  { id :: Int
  , name :: String
  , teamId :: Int
  , position :: String
  }

playerData :: Array Player
playerData = 
  [ { id: 408234, name: "Miguel Cabrera", teamId: 116, position: "10" }
  , { id: 425794, name: "Adam Wainwright", teamId: 138, position: "1" }
  -- ... other players
  ]

ui :: forall query input output m. H.Component query input output m
ui =
  H.mkComponent
    { initialState: identity
    , render
    , eval: H.mkEval H.defaultEval
    }

  where
  playerRow :: Player -> HH.HTML m Unit
  playerRow player =
      HH.tr_
        [ HH.td_ [ HH.text $ show player.id ]
        , HH.td_ [ HH.text player.name ]
        , HH.td_ [ HH.text $ show player.teamId ]
        , HH.td_ [ HH.text player.position ]
        ]

  render :: forall state. state -> H.ComponentHTML query input m
  render _ =
      HH.div_
        [ HH.table_
            ( [ HH.thead_
                  [ HH.tr_
                      [ HH.th_ [ HH.text "ID" ]
                      , HH.th_ [ HH.text "Name" ]
                      , HH.th_ [ HH.text "Team ID" ]
                      , HH.th_ [ HH.text "Position" ]
                      ]
                  ]
              ] <> map playerRow playerData
            )
        ]


main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui unit body
