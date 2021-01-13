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
import Hey.Pages.About (mkAboutPage)
import Hey.Pages.Github (mkGithubPage)
import Hey.Pages.Landing (mkLandingPage)
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

mkPage :: Component PageProps
mkPage =
  component "Page"
    $ \{ env, route, children } -> React.do
        thresholdRef <- useRef null
        entry <- Observer.useIntersectionObserverEntry thresholdRef
        let
          ratio = maybe 0.0 intersectionRatio entry
        -- Update active route when this page becomes visible
        useEffect ratio
          $ do
              if ratio == 1.0 then
                env.router.replace route
              else
                pure unit
              pure mempty
        pure
          $ DOM.section
              { id: show route
              , className: styles.page
              , children:
                  [ DOM.div { ref: thresholdRef, className: styles.threshold } ]
                    <> children
              }

spacer :: JSX
spacer = DOM.div { style: DOM.css { width: "100vw", height: "100vh" } }

mkRoutes :: Effect (Array (Route /\ (Env -> JSX)))
mkRoutes = do
  landingPage <- mkLandingPage
  aboutPage <- mkAboutPage
  githubPage <- mkGithubPage
  pure
    $ [ Home /\ const spacer
      , About /\ aboutPage
      , Projects /\ githubPage
      , End /\ const spacer
      ]

mkHomePage :: Component Env
mkHomePage = do
  observerProvider <- Observer.mkIntersectionObserverProvider $ observerOpts
  routes <- mkRoutes
  page <- mkPage
  component "Home"
    $ \env ->
        pure
          $ observerProvider
          $ pure
          $ DOM.div
              { id: "scroller"
              , className:
                  styles.container
              , children: routes # map \(route /\ c) -> page { env, route, children: [ c env ] }
              }
  where
  observerOpts = Observer.defOptions { threshold = [ 1.0 ] }
