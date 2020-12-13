module Hey.Components.Github.Repo (mkRepo) where

import Prelude
import Data.Foldable (foldl)
import Data.Maybe (maybe)
import Data.Nullable (null)
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Hey.Api.Github (Repo)
import Hey.Hooks.UseIntersectionObserver (useIntersectionObserverEntry)
import Hey.Styles (VisibilityStyles, PositionStyles, (.&))
import Hey.Styles as Cls
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffectOnce, useRef)
import React.Basic.Hooks as React
import Type.Row (type (+))
import Web.DOM as DOM

foreign import styles :: Styles

type Styles
  = { container :: String
    , repo :: String
    }

mkRepo :: Component Repo
mkRepo =
  component "GithubRepo"
    $ \repo -> React.do
        pure
          $ DOM.div
              { className: styles.repo
              , children:
                  [ DOM.text repo.name
                  ]
              }
