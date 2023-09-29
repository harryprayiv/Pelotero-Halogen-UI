module Main where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Effect.Aff (Aff)

type Player = 
  { playerId :: Int
  , name :: String
  , teamId :: Int
  , position :: String
  }

playerData :: Array Player
playerData = 
  [ { playerId: 408234, name: "Miguel Cabrera", teamId: 116, position: "10" }
  , { playerId: 425794, name: "Adam Wainwright", teamId: 138, position: "1" }
  -- ... other players
  ]

data Action = Initialize

data Query a

type State = { players :: Array Player }

ui :: forall m. H.Component HH.HTML Query Unit m
ui =
  H.mkComponent
    { initialState: \_ -> { players: playerData }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

  where
  playerRow :: Player -> HH.HTML m Action
  playerRow player =
      HH.tr_
        [ HH.td_ [ HH.text $ show player.playerId ]
        , HH.td_ [ HH.text player.name ]
        , HH.td_ [ HH.text $ show player.teamId ]
        , HH.td_ [ HH.text player.position ]
        ]

  render :: State -> HH.HTML m Action
  render state =
      HH.div_
        [ HH.table_
            ( [ HH.thead_
                  [ HH.tr_
                      [ HH.th_ [ HH.text "PlayerID" ]
                      , HH.th_ [ HH.text "Name" ]
                      , HH.th_ [ HH.text "Team ID" ]
                      , HH.th_ [ HH.text "Position" ]
                      ]
                  ]
              ] <> map playerRow state.players
            )
        ]

  handleAction :: Action -> H.HalogenM State Query () m Unit
  handleAction _ = pure unit

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI ui unit body
