module Hey.Pages.About
  ( mkAboutPage
  ) where

import Prelude
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Hey.Api.Github (fetchViewer)
import Hey.Components.Github as GH
import Hey.Hooks.UseFetch (useFetch)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, fragment)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { page :: String
    }

mkAboutPage :: forall a. Component a
mkAboutPage =
  component "About" \_ -> React.do
    useFetch fetchViewer
      :>>= case _ of
          Nothing -> pure $ DOM.text "loading..."
          Just { data: res } -> do
            pure
              $ DOM.div
                  { className: styles.page
                  , children: [ DOM.text "lmao" ]
                  }
