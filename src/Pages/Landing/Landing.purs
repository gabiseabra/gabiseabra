module Hey.Pages.Landing
  ( mkLandingPage
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Hey.Components.Hole (hole)
import Hey.Components.PerspectiveText (mkPerspectiveText)
import Hey.Components.Section (mkSection)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

mkLandingPage :: forall a. Component a
mkLandingPage = do
  section <- mkSection
  component "Landing" \_ ->
    pure
      $ section
          { key: "home"
          , height: "100vh"
          , snap: Nothing
          , children: pure $ DOM.div { style: DOM.css { background: "blue", height: "100%" } }
          }
