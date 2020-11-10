module Hey.Components.SVG.Filters where

import React.Basic (JSX)
import React.Basic.DOM.SVG as SVG

goo :: String -> JSX
goo id =
  SVG.filter
    { id: id
    , children:
      [ SVG.feGaussianBlur
        { "in": "SourceGraphic"
        , stdDeviation: "40"
        , result: "blur"
        }
      , SVG.feColorMatrix
        { "in": "blur"
        , "type": "matrix"
        , values: "1 0 0 0 0  0 1 0 0 0  0 0 1 0 0  0 0 0 75 -10"
        , result: "goo"
        }
      , SVG.feBlend
        { "in": "SourceGraphic"
        , in2: "goo"
        }
      ]
    }
