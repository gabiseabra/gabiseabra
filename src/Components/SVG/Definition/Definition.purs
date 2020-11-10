module Hey.Components.SVG.Definition where

import Prelude

import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG

foreign import styles :: Styles

type Styles =
  { definition :: String
  }

def :: Int -> Int -> Array JSX -> JSX
def width height children =
  SVG.svg
    { className: styles.definition
    , viewBox: "0 0 " <> (show width) <> " " <> (show height)
    , enableBackground: "0 0 " <> (show width) <> " " <> (show height)
    , children: [ SVG.defs { children } ]
    }
