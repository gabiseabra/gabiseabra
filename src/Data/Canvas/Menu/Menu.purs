module Hey.Data.Canvas.Menu
  ( Menu
  , Link
  , mkCanvas
  , setScroller
  ) where

import Prelude
import Effect (Effect)
import Hey.Data.Canvas (kind Scene, Canvas)
import Hey.Hooks.UseScroller (Scroller)

type Link
  = { id :: String
    , label :: String
    , onClick :: Effect Unit
    }

foreign import data Menu :: Scene

foreign import mkCanvas :: Array Link -> Effect (Canvas Menu)

foreign import setScroller :: Canvas Menu -> Scroller -> Effect Unit
