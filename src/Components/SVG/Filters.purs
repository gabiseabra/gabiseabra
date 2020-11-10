module Hey.Components.SVG.Filters where

import Prelude

import Data.Foldable (foldl)
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

animflickerOffset :: Array Int -> Int -> JSX
animflickerOffset offsets d =
  let foldValues acc i = acc <> ";" <> show (d - i)
  in SVG.animate
    { attributeName: "dx"
    , dur: "0.5s"
    , values: foldl foldValues (show d) offsets
    , repeatCount: "indefinite"
    }

anaglyph :: Int -> String -> JSX
anaglyph d id =
  SVG.filter
    { id: id
    , filterUnits: "userSpaceOnUse"
    , children:
      [ SVG.feOffset
        { "in": "SourceAlpha"
        , dx: show d
        , dy: "0"
        , result: "L1"
        , children: [ animflickerOffset [ 2, 0, 1, 3 ] d ]
        }
      , SVG.feOffset
        { "in": "SourceAlpha"
        , dx: show (-d)
        , dy: "0"
        , result: "R1"
        , children: [ animflickerOffset [ 1, 2, 1, 3 ] (-d) ]
        }
      , SVG.feComponentTransfer
        { "in": "L1"
        , result: "L2"
        , children:
          [ SVG.feFuncR { type: "table", tableValues: "1 1 1 1" }
          , SVG.feFuncG { type: "table", tableValues: "0 0 0 1" }
          , SVG.feFuncB { type: "table", tableValues: "0 0 0 1" }
          ]
        }
      , SVG.feComponentTransfer
        { "in": "R1"
        , result: "R2"
        , children:
          [ SVG.feFuncR { type: "table", tableValues: "0 0 0 1" }
          , SVG.feFuncG { type: "table", tableValues: "0 0 0 1" }
          , SVG.feFuncB { type: "table", tableValues: "1 1 1 1" }
          ]
        }
      , SVG.feComposite
        { operator: "arithmetic"
        , "in": "L2"
        , in2: "R2"
        , k1: "0.5"
        , k2: "0.5"
        , k3: "0.5"
        }
      , SVG.feMerge_
        [ SVG.feMergeNode { "in": "L2"}
        , SVG.feMergeNode { "in": "R2"}
        , SVG.feMergeNode { "in": "SourceGraphic"}
        ]
      ]
    }