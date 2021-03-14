module Hey.Components.Menu where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Menu as Menu
import Hey.Data.Route (Route(..))
import Hey.Extra.DOM (scrollIntoView)
import Hey.Hooks.UseScroller (useScroller)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Win

foreign import styles :: Styles

type Styles
  = { nav :: String
    }

pushRoute :: Route -> Effect Unit
pushRoute route =
  HTML.window
    >>= Win.document
    >>= HTMLDocument.toNonElementParentNode
    >>> NEPN.getElementById (show route)
    >>= maybe (pure unit) scrollIntoView

links :: Array Menu.Link
links =
  [ { label: "HOME", route: Home }
  , { label: "ABOUT", route: About }
  , { label: "CODE", route: Projects }
  ]
    # map \{ label, route } ->
        { label
        , id: show route
        , onClick: pushRoute route
        }

mkMenu :: forall a. Component a
mkMenu =
  component "Menu"
    $ \_ -> React.do
        ref <- useRef null
        canvas /\ setCanvas <- useState Nothing
        scroller <- useScroller
        -- instantiate canvas
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \nav -> do
              c <- Menu.mkCanvas $ links
              setCanvas $ const $ Just c
              void $ Node.appendChild (Canvas.toNode c) nav
              pure
                $ do
                    setCanvas $ const Nothing
                    void $ Node.removeChild (Canvas.toNode c) nav
                    Canvas.destroy c
        -- the scroller has info on the actve route
        useEffect scroller
          $ maybe (mempty) (flip Menu.setScroller $ scroller) canvas
          *> pure mempty
        pure
          $ DOM.nav { ref, className: styles.nav }
