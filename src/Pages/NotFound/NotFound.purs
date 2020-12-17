module Hey.Pages.NotFound
  ( mkNotFoundPage
  ) where

import Prelude
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { page :: String
    , content :: String
    }

mkNotFoundPage :: forall a. Component a
mkNotFoundPage =
  component "NotFound" \_ -> React.do
    pure
      $ DOM.div
          { className: styles.page
          , children:
              [ DOM.div
                  { className: styles.content
                  , children: [ DOM.text "404" ]
                  }
              ]
          }
