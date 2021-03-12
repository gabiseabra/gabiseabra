module Hey.Pages.Spacer
  ( mkSpacerPage
  ) where

import Prelude
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

mkSpacerPage :: forall a. Component a
mkSpacerPage = do
  component "Spacer" \_ -> React.do
    pure
      $ DOM.section
          { style:
              DOM.css
                { height: "100vh"
                , width: "100vh"
                }
          }
