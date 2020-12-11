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
import Hey.Hooks.UseIntersectionObserver (useIntersectionObserverEntry)
import Hey.Pages.Github (mkGithubPage)
import Hey.Pages.Landing (mkLandingPage)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useRef)
import React.Basic.Hooks as React
import Web.IntersectionObserverEntry (intersectionRatio)
import Wire.React (useSignal)

foreign import styles :: Styles

type Styles
  = { threshold :: String
    }

mkThreshold :: Component (Env /\ Route)
mkThreshold =
  component "Landing"
    $ \(env /\ route) -> React.do
        ref <- useRef null
        entry <- useIntersectionObserverEntry ref
        currentRoute <- useSignal env.router.signal
        let
          ratio = maybe 0.0 intersectionRatio entry
        useEffect ratio
          $ do
              if ratio == 1.0 && route /= currentRoute then
                env.router.replace route
              else
                pure unit
              pure mempty
        pure $ DOM.div { ref, className: styles.threshold }

mkRoutes :: Effect (Array { route :: Route, component :: Env -> JSX })
mkRoutes = do
  githubPage <- mkGithubPage
  landingPage <- mkLandingPage
  pure
    $ [ { route: Home, component: landingPage }
      , { route: About, component: githubPage }
      ]

mkHomePage :: Component Env
mkHomePage = do
  routes <- mkRoutes
  threshold <- mkThreshold
  component "Home"
    $ \env -> React.do
        pure $ fragment $ routes
          # map \{ route, component: c } ->
              fragment [ threshold (env /\ route), c env ]
