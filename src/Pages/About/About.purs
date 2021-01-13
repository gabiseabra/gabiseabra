module Hey.Pages.About
  ( mkAboutPage
  ) where

import Prelude
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Hey.Api.Github (fetchViewer)
import Hey.Hooks.UseFetch (useFetch)
import Hey.Hooks.UseScroll (usePerspective, useSnapPointRef)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { page :: String
    }

mkAboutPage :: forall a. Component a
mkAboutPage =
  component "About" \_ -> React.do
    ref <- useRef null
    style <- usePerspective ref
    useSnapPointRef "home" ref
    children <-
      useFetch fetchViewer
        :>>= case _ of
            Nothing -> pure [ DOM.text "loading..." ]
            Just { data: res } -> pure [ DOM.text "lmaooo" ]
    pure $ DOM.div { className: styles.page, ref, style, children }
