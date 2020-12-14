module Hey.Components.Hole where

import Prelude

import Data.Unfoldable (replicate)
import Hey.Components.SVG.Definition (def)
import Hey.Components.SVG.Filters as Filter
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG

foreign import styles :: Styles

type Styles
  = { container :: String
    , blob :: String
    , glow :: String
    , ring :: String
    }


svgDefs :: JSX
svgDefs = def 100 100
  [ Filter.goo "goo"
  ]

blob :: JSX
blob = DOM.div
  { className: styles.blob
  , children: replicate 3 $ DOM.div_ []
  }

ring :: JSX
ring = DOM.div
  { className: styles.ring
  , children: replicate 2 $ DOM.div_ []
  }

glow :: JSX
glow = DOM.div
  { className: styles.glow
  , children: replicate 2 $ DOM.div_ []
  }

hole :: JSX
hole = fragment
    [ glow
    , ring
    , blob
    , svgDefs
    ]

