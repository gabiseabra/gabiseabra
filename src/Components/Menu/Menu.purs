module Hey.Components.Menu where

import Prelude
import Data.Foldable (intercalate)
import Data.Maybe (maybe)
import Data.Monoid (guard)
import Data.String (length)
import Effect (Effect)
import Hey.Components.SVG.Definition (def)
import Hey.Components.SVG.Filters (anaglyph)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..), href)
import Hey.Extra.DOM (scrollIntoView)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.Events (preventDefault)
import React.Basic.DOM.SVG as SVG
import React.Basic.Events (handler)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Wire.React (useSignal)

foreign import styles :: Styles

type Styles
  = { nav :: String
    , link :: String
    , active :: String
    , menu :: String
    , lmao :: String
    }

type LinkProps
  = { label :: String
    , href :: String
    , active :: Boolean
    , onClick :: Effect Unit
    }

svgDefs = def 100 100 [ anaglyph 5 "anaglyph" ] :: JSX

linkViewBox :: String -> String
linkViewBox lbl = "0 0 " <> (show $ length lbl * 20 + 20) <> " 60"

link :: LinkProps -> JSX
link { label, href, active, onClick } =
  DOM.a
    { href
    , className:
        intercalate " "
          $ pure styles.link
          <> guard active [ styles.active ]
          <> guard active [ styles.lmao ]
    , onClick: handler preventDefault $ const onClick
    , children:
        [ SVG.svg
            { viewBox: linkViewBox label
            , children:
                [ SVG.text
                    { x: "50%"
                    , y: "50%"
                    , dominantBaseline: "middle"
                    , textAnchor: "middle"
                    , children: [ DOM.text label ]
                    }
                ]
            }
        ]
    }

mkMenu :: Component Env
mkMenu =
  component "Menu"
    $ \env -> React.do
        currentRoute <- useSignal env.router.signal
        let
          links =
            [ { label: "HOME", route: Home }
            , { label: "ABOUT", route: About }
            ]
              # map \{ label, route } ->
                  link
                    { label
                    , href: href route
                    , active: route == currentRoute
                    , onClick:
                        do
                          env.router.push route
                          window
                            >>= document
                            >>= toNonElementParentNode
                            >>> getElementById (show route)
                            >>= maybe (pure unit) scrollIntoView
                    }
        pure
          $ DOM.nav
              { className: styles.nav
              , children: links <> [ svgDefs ]
              }
