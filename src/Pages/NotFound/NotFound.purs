module Hey.Pages.NotFound
  ( mkNotFoundPage
  ) where

import Prelude

import Hey.Components.SVG.Blob (blob)
import Hey.Components.SVG.Definition (def)
import Hey.Components.SVG.Filters (goo)
import Hey.Hooks.UseViewportSize (ViewportSize, useViewportSize)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles =
  { page :: String
  , content :: String
  }

svgDefs :: ViewportSize -> JSX
svgDefs { width, height } = def width height
  [ SVG.clipPath
    { id: "big-blob"
    , clipPathUnits: "objectBoundingBox"
    , children: [ blob ]
    }
  , goo "goo"
  ]

mkNotFoundPage :: forall a . Component a
mkNotFoundPage = component "NotFound" \_ -> React.do
  viewport <- useViewportSize
  pure
    $ DOM.div
    { className: styles.page
    , children:
      [ DOM.div
        { className: styles.content
        , children: [ DOM.text "404" ]
        }
      , svgDefs viewport
      ]
    }
  