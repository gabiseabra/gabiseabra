module Hey.Components.SVG.Blob where

import Prelude

import React.Basic (JSX, fragment)
import React.Basic.DOM.SVG as SVG
import Record (merge)

foreign import styles :: Styles

type Styles =
  { move0 :: String
  , move1 :: String
  , move2 :: String
  , move3 :: String
  }

defOptions = merge
  { cx: "0.5"
  , cy: "0.5"
  , fill: "black"
  , stroke: "black"
  }

blob :: JSX
blob =
  fragment
    [ SVG.circle
        $ defOptions
        { r: ".35"
        , className: styles.move0
        }
    , SVG.circle
        $ defOptions
        { r: ".3"
        , className: styles.move1
        }
    , SVG.circle
        $ defOptions
        { r: ".25"
        , className: styles.move2
        }
    , SVG.circle
        $ defOptions
        { r: ".1"
        , className: styles.move3
        }
    ]
