module Hey.Canvas.Background (mkCanvas) where

import Effect (Effect)
import Web.DOM (Node)

foreign import mkCanvas :: Effect (Node)
