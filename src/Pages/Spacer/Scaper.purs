module Hey.Pages.Spacer
  ( mkSpacerPage
  ) where

import Prelude
import Data.Nullable (null)
import Hey.Hooks.UseScroll (useSnapPoint)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

mkSpacerPage :: forall a. Component a
mkSpacerPage = do
  component "Spacer" \_ -> React.do
    ref <- useRef null
    useSnapPoint ref
    pure
      $ DOM.section
          { ref
          , style:
              DOM.css
                { background: "#0000ff50"
                , height: "100vh"
                , width: "100vh"
                }
          }
