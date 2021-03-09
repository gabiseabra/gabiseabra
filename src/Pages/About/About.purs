module Hey.Pages.About
  ( mkAboutPage
  ) where

import Prelude
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Hey.API.Github (fetchViewer)
import Hey.Components.Github.Stats (mkStats)
import Hey.Data.Env (Env)
import Hey.Hooks.UseFetch (useFetch)
import Hey.Hooks.UseScroll (useSnapPoint)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { page :: String
    }

mkAboutPage :: Component Env
mkAboutPage = do
  stats <- mkStats
  component "About" \{ options } -> React.do
    ref <- useRef null
    useSnapPoint ref
    children <-
      useFetch (fetchViewer options.github.token)
        :>>= case _ of
            Nothing -> pure [ DOM.text "loading..." ]
            Just { data: res } -> pure [ stats res.viewer ]
    pure
      $ DOM.section
          { ref
          , style: DOM.css { height: "75vh" }
          , children
          }
