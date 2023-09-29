module Drafting.Main (drafting) where

import Prelude (($), Unit)
import Effect (Effect)
import Drafting (render, form)

foreign import setHTML :: String -> Effect Unit

drafting :: Effect Unit
drafting = setHTML $ render form