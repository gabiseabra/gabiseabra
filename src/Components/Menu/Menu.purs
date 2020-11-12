module Hey.Components.Menu where

import Prelude

import Data.String (length)
import Hey.Components.SVG.Definition (def)
import Hey.Components.SVG.Filters (anaglyph)
import Hey.Env (Env)
import Hey.Router (Route(..), href)
import Hey.Extra.Styles ((.&), (?&))
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Wire.React (useSignal)

foreign import styles :: Styles

type Styles =
  { nav :: String
  , link :: String
  , active :: String
  , menu :: String
  }

type LinkProps =
  { label :: String
  , href :: String
  , active :: Boolean
  }

svgDefs = def 100 100 [ anaglyph 5 "anaglyph" ] :: JSX

linkViewBox :: String -> String
linkViewBox lbl =
  "0 0 " <> (show $ length lbl * 20 + 20) <> " 60"

link :: LinkProps -> JSX
link { label, href, active } = DOM.a
  { href
  , className: styles.link .& active ?& styles.active
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

menu :: Route -> JSX
menu route = DOM.nav
    { className: styles.nav
    , children:
      [ link { label: "HOME", href: href Home, active: Home == route }
      , link { label: "ABOUT", href: href About, active: About == route }
      , svgDefs
      ]
    }

mkMenu :: Component Env
mkMenu = component "Menu" $ \env -> React.do
  route <- useSignal env.router.signal
  pure $ menu route
