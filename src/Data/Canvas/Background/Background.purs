module Hey.Data.Canvas.Background (Background, mkCanvas) where

import Effect (Effect)
import Hey.Data.Canvas (kind Scene, Canvas)

foreign import data Background :: Scene

foreign import mkCanvas :: Effect (Canvas Background)
