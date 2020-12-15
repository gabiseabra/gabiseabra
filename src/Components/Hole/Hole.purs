module Hey.Components.Hole (hole) where

import Prelude

import Data.Unfoldable (replicate)
import React.Basic (JSX)
import React.Basic.DOM as DOM

foreign import styles :: Styles

type Styles
  = { container :: String
    , glow :: String
    , ring :: String
    }

ring :: JSX
ring = DOM.div
  { className: styles.ring
  , children: replicate 2 $ DOM.div_ []
  }

glow :: JSX
glow = DOM.div
  { className: styles.glow
  , children: replicate 3 $ DOM.div_ []
  }

hole :: JSX
hole = DOM.div
  { className: styles.container
  , children: [ glow, ring ]
  }
