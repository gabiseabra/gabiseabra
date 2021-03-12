module Hey.Data.Canvas.Background (Background, mkCanvas) where

import Prelude
import Effect (Effect)
import Hey.Data.Canvas (kind Scene, Canvas)
import Hey.Hooks.UseScroller (Scroller)

foreign import data Background :: Scene

foreign import mkCanvas :: Effect (Canvas Background)

foreign import setScroller :: Canvas Background -> Scroller -> Effect Unit
