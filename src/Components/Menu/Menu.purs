module Hey.Components.Menu where

import Prelude
import Data.Maybe (maybe)
import Data.Nullable (null, notNull)
import Effect (Effect)
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Menu as Menu
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..), href)
import Hey.Hooks.UseScroll (snapTo)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useEffectOnce, useRef, writeRef)
import React.Basic.Hooks as React
import Web.DOM.Node (appendChild, removeChild)
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Win
import Wire.React (useSignal)

foreign import styles :: Styles

foreign import attachMouseMove :: ({ x :: Number, y :: Number } -> Effect Unit) -> Effect (Effect Unit)

type Styles
  = { nav :: String
    }

pushRoute :: Route -> Effect Unit
pushRoute route =
  window
    >>= Win.document
    >>= HTMLDocument.toNonElementParentNode
    >>> NEPN.getElementById (show route)
    >>= (=<<) HTMLElement.fromElement
    >>> maybe (pure unit) snapTo

links :: Array Menu.Link
links =
  [ { label: "HOME", route: Home }
  , { label: "ABOUT", route: About }
  , { label: "CODE", route: Projects }
  ]
    # map \{ label, route } ->
        { label
        , id: href route
        , onClick: pushRoute route
        }

mkMenu :: Component Env
mkMenu =
  component "Menu"
    $ \env -> React.do
        ref <- useRef null
        canvas <- useRef null
        currentRoute <- useSignal env.router.signal
        -- instantiate canvas
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \nav -> do
              c <- Menu.mkCanvas $ links
              writeRef canvas $ notNull c
              void $ appendChild (Canvas.toNode c) nav
              pure
                $ do
                    writeRef canvas null
                    void $ removeChild (Canvas.toNode c) nav
                    Canvas.destroy c
        -- update active link
        useEffect currentRoute
          $ readRefMaybe canvas
          >>= maybe (pure unit) (flip Menu.setActive $ show currentRoute)
          *> pure mempty
        pure
          $ DOM.nav { ref, className: styles.nav }
