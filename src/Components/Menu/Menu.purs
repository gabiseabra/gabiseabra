module Hey.Components.Menu where

import Prelude
import Data.Maybe (maybe)
import Data.Nullable (null, notNull)
import Effect (Effect)
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Menu as Menu
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Extra.DOM (scrollIntoView)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useEffectOnce, useRef, writeRef)
import React.Basic.Hooks as React
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Win
import Wire.React (useSignal)

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
              void $ Node.appendChild (Canvas.toNode c) nav
              pure
                $ do
                    writeRef canvas null
                    void $ Node.removeChild (Canvas.toNode c) nav
                    Canvas.destroy c
        -- update active link
        useEffect currentRoute
          $ readRefMaybe canvas
          >>= maybe (pure unit) (flip Menu.setActive $ show currentRoute)
          *> pure mempty
        pure
          $ DOM.nav { ref, className: styles.nav }
