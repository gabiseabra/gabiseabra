module Hey.Pages.Landing
  ( mkLandingPage
  ) where

import Prelude

import Hey.Components.Hole (hole)
import Hey.Hooks.UseViewportSize (ViewportSize, useViewportSize)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles =
  { page :: String
  }


mkLandingPage :: forall a . Component a
mkLandingPage = component "Landing" \_ -> React.do
  viewport <- useViewportSize
  pure
    $ DOM.div
    { className: styles.page
    , children: [ hole ]
    }
  