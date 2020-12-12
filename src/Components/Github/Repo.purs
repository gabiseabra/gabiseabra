module Hey.Components.Github.Repo (mkRepo) where

import Prelude
import Data.Foldable (foldl)
import Data.Maybe (maybe)
import Data.Nullable (null)
import Hey.Api.Github (Repo)
import Hey.Hooks.UseIntersectionObserver (useIntersectionObserverEntry)
import Hey.Styles (VisibilityStyles, PositionStyles, (.&))
import Hey.Styles as Cls
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React
import Type.Row (type (+))

foreign import styles :: Styles

type Styles
  = Record
      ( VisibilityStyles + PositionStyles
          + ( container :: String
          , repo :: String
          )
      )

mkRepo :: Component Repo
mkRepo =
  component "GithubRepo"
    $ \repo -> React.do
        ref <- useRef null
        entry <- useIntersectionObserverEntry ref
        pure
          $ DOM.div
              { ref
              , className: styles.container
              , children:
                  pure
                    $ DOM.div
                        { className:
                            foldl (.&)
                              mempty
                              [ styles.repo
                              , maybe mempty (Cls.position styles) entry
                              , maybe mempty (Cls.visibility styles) entry
                              ]
                        , children:
                            [ DOM.text repo.name
                            ]
                        }
              }
