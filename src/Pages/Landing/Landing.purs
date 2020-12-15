module Hey.Pages.Landing
  ( mkLandingPage
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Hey.Components.Hole (hole)
import Hey.Components.PerspectiveText (mkPerspectiveText)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles =
  { page :: String
  , background :: String
  , content :: String
  , smoke :: String
  , hole :: String
  , hole2 :: String
  , text :: String
  }

mkLandingPage :: forall a . Component a
mkLandingPage = do
  perspectiveText <- mkPerspectiveText
  component "Landing" \_ -> React.do
    ref <- useRef null
    pure
      $ DOM.div
      { ref
      , className: styles.page
      , children:
        [ DOM.div
          { className: styles.background
          , children:
            [ DOM.img { className: styles.smoke, src: "/img/smoke.gif" }
            , DOM.div { className: styles.hole, children: [ hole ] }
            , DOM.div { className: styles.hole2 }
            ]
          }
        , DOM.div
          { className: styles.text
          , children: pure $ perspectiveText
              { targetRef: Just ref
              , before: Just "Hi, I'm"
              , text: "Gabi Seabra"
              , after: Just ", software developer"
              }
          }
        ]
      }
    
