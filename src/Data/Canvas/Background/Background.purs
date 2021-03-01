module Hey.Data.Canvas.Background (Background, mkCanvas) where

import Effect (Effect)
import Hey.Data.Canvas (kind CanvasElement, Canvas)

foreign import data Background :: CanvasElement

foreign import mkCanvas :: Effect (Canvas Background)
