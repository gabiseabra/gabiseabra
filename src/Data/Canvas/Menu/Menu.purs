module Hey.Data.Canvas.Menu
  ( Menu
  , Link
  , mkCanvas
  , setLinks
  ) where

import Prelude
import Effect (Effect)
import Hey.Data.Canvas (kind Scene, Canvas)

type Link
  = { id :: String
    , label :: String
    , active :: Boolean
    , onClick :: Effect Unit
    }

foreign import data Menu :: Scene

foreign import mkCanvas :: Effect (Canvas Menu)

foreign import setLinks :: Canvas Menu -> Array Link -> Effect Unit
