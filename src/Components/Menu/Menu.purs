module Hey.Components.Menu where

import Prelude

import Data.String (length)
import Hey.Styles ((?&))
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG

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

linkViewBox :: String -> String
linkViewBox lbl =
  "0 0 " <> (show $ length lbl * 20 + 20) <> " 60"

link :: LinkProps -> JSX
link { label, href, active } = DOM.a
  { href
  , className: styles.link <> active ?& styles.active
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

menu :: JSX
menu =
  DOM.nav
    { className: styles.nav
    , children:
      [ link { label: "HOME", href: "#", active: true }
      , link { label: "SKILLZ", href: "#", active: false }
      , link { label: "PROJECTS", href: "#", active: false }
      ]
    }
