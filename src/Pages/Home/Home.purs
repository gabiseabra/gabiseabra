module Hey.Pages.Home
  ( mkHomePage
  ) where

import Prelude
import Data.Maybe (maybe)
import Data.Nullable (null)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseIntersectionObserver as Observer
import Hey.Hooks.UseScroll (mkScrollProvider)
import Hey.Pages.About (mkAboutPage)
import Hey.Pages.Github (mkGithubPage)
import Hey.Pages.Spacer (mkSpacerPage)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useRef)
import React.Basic.Hooks as React
import Web.IntersectionObserverEntry (intersectionRatio)

foreign import styles :: Styles

type Styles
  = { container :: String
    , page :: String
    , threshold :: String
    }

type PageProps
  = { env :: Env
    , route :: Route
    , children :: Array JSX
    }

spacer :: JSX
spacer = DOM.div { style: DOM.css { width: "100vw", height: "100vh" } }

mkRoutes :: Effect (Array (Route /\ (Env -> JSX)))
mkRoutes = do
  spacerPage <- mkSpacerPage
  aboutPage <- mkAboutPage
  githubPage <- mkGithubPage
  pure
    $ [ Home /\ \_ -> spacerPage "home"
      , About /\ aboutPage
      , Projects /\ githubPage
      , End /\ \_ -> spacerPage "end"
      ]

mkHomePage :: Component Env
mkHomePage = do
  routes <- mkRoutes
  scrollProvider <- mkScrollProvider
  component "Home"
    $ \env -> pure $ scrollProvider $ routes <#> \(route /\ c) -> c env
