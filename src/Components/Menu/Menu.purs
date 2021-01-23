module Hey.Components.Menu where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (maybe)
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Foreign.Object as Object
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..), href)
import Hey.Hooks.UseScroll (snapTo)
import React.Basic (JSX)
import React.Basic.DOM (CSS)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, Hook, UseEffect, component, useEffectOnce, useState)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode as NEPN
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (eventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Win
import Wire.React (useSignal)

foreign import styles :: Styles

foreign import attachMouseMove :: ({ x :: Number, y :: Number } -> Effect Unit) -> Effect (Effect Unit)

type Styles
  = { nav :: String
    , link :: String
    , active :: String
    , menu :: String
    , textForeground :: String
    , textBackground :: String
    }

type LinkProps
  = { label :: String
    , href :: String
    , active :: Boolean
    , onClick :: Effect Unit
    }

lerp :: Number -> Number -> Number -> Number
lerp v0 v1 t = v0 * (1.0 - t) + v1 * t

pushRoute :: Route -> Effect Unit
pushRoute route =
  window
    >>= Win.document
    >>= HTMLDocument.toNonElementParentNode
    >>> NEPN.getElementById (show route)
    >>= (=<<) HTMLElement.fromElement
    >>> maybe (pure unit) snapTo

calcPerspectiveOrigin :: { x :: Number, y :: Number } -> String
calcPerspectiveOrigin { x, y } = originX <> "% 125%"
  where
  originX = show $ (+) 50.0 $ lerp (-5.0) 5.0 x

calcTransform :: { x :: Number, y :: Number } -> String
calcTransform { x, y } = "rotateX(" <> dy <> "deg)"
  where
  dy = show $ (+) 2.0 $ lerp (-2.0) 4.0 y

link :: LinkProps -> JSX
link { label, href, active, onClick } =
  DOM.a
    { href
    , className:
        intercalate " "
          $ pure styles.link
          <> guard active [ styles.active ]
    , onClick: handler preventDefault $ const onClick
    , children: [ bg, bg, fg ]
    }
  where
  text = [ DOM.text label ]

  fg = DOM.span { className: styles.textForeground, children: text }

  bg =
    DOM.span
      { _aria: Object.fromHomogeneous { hidden: "true" }
      , className: styles.textBackground
      , children: text
      }

mkMenu :: Component Env
mkMenu =
  component "Menu"
    $ \env -> React.do
        navStyle /\ setNavStyle <- useState mempty
        pivotStyle /\ setPivotStyle <- useState mempty
        useEffectOnce $ attachMouseMove
          $ \st -> do
              setPivotStyle $ const $ DOM.css { transform: calcTransform st }
              setNavStyle $ const $ DOM.css { perspectiveOrigin: calcPerspectiveOrigin st }
        currentRoute <- useSignal env.router.signal
        let
          links =
            [ { label: "HOME", route: Home }
            , { label: "ABOUT", route: About }
            , { label: "PROJECTS", route: Projects }
            ]
              # map \{ label, route } ->
                  link
                    { label
                    , href: href route
                    , active: route == currentRoute
                    , onClick: pushRoute route
                    }
        pure
          $ DOM.nav
              { style: navStyle
              , className: styles.nav
              , children:
                  pure
                    $ DOM.div
                        { style: pivotStyle, children: links
                        }
              }
