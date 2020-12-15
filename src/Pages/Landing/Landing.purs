module Hey.Pages.Landing
  ( mkLandingPage
  ) where

import Prelude

import Hey.Components.Hole (hole)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles =
  { page :: String
  , smoke :: String
  }

mkLandingPage :: forall a . Component a
mkLandingPage = component "Landing" \_ -> React.do
  pure
    $ DOM.div
    { className: styles.page
    , children:
      [ DOM.img { className: styles.smoke, src: "/img/smoke.gif" }
      , hole
      ]
    }
  