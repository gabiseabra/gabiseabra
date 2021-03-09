module Hey.Extra.DOM where

import Prelude
import Effect (Effect)
import Web.DOM (Element)

foreign import scrollIntoView :: Element -> Effect Unit
