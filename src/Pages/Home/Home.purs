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
import Hey.Hooks.UseSnapPoints (useSnapPoint)
import Hey.Pages.Github (mkGithubPage)
import Hey.Pages.Landing (mkLandingPage)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffect, useRef)
import React.Basic.Hooks as React
import Web.HTML.HTMLElement as HTMLElement
import Web.IntersectionObserverEntry (intersectionRatio)

foreign import styles :: Styles

type Styles
  = { page :: String
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
        pageRef <- useRef null
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
        -- Use the beggining of this section as a snap point
        useSnapPoint (show route)
          $ readRefMaybe pageRef
          >>= ((=<<) HTMLElement.fromNode)
          >>> maybe (pure []) (HTMLElement.offsetTop >=> (pure >>> pure))
        pure
          $ DOM.section
              { ref: pageRef
              , className: styles.page
              , children:
                  [ DOM.div { ref: thresholdRef, className: styles.threshold } ]
                    <> children
              }

mkRoutes :: Effect (Array (Route /\ (Env -> JSX)))
mkRoutes = do
  githubPage <- mkGithubPage
  landingPage <- mkLandingPage
  pure
    $ [ Home /\ landingPage
      , About /\ githubPage
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
          $ routes
          # map \(route /\ c) ->
              page { env, route, children: [ c env ] }
  where
  observerOpts = Observer.defOptions { threshold = [ 1.0 ] }
