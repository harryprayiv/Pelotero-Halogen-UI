module Drafting.Main (drafting) where

import Prelude (($), Unit)
import Effect (Effect)
import Console (render, form)

foreign import setHTML :: String -> Effect Unit

drafting :: Effect Unit
drafting = setHTML $ render form