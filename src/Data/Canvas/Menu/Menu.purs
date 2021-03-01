module Hey.Data.Canvas.Menu
  ( Menu
  , Link
  , mkCanvas
  , setActive
  ) where

import Prelude
import Effect (Effect)
import Hey.Data.Canvas (kind Scene, Canvas)

type Link
  = { id :: String
    , label :: String
    , onClick :: Effect Unit
    }

foreign import data Menu :: Scene

foreign import mkCanvas :: Array Link -> Effect (Canvas Menu)

foreign import setActive :: Canvas Menu -> String -> Effect Unit
