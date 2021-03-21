module Hey.Pages.Spacer
  ( mkSpacerPage
  ) where

import Prelude
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)

mkSpacerPage :: forall a. Component a
mkSpacerPage = do
  component "Spacer" \_ -> React.do
    pure
      $ DOM.section
          { style:
              DOM.css
                { height: "100vh"
                , width: "100vw"
                }
          }
