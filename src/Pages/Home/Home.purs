module Hey.Pages.Home
  ( mkHomePage
  ) where

import Prelude
import Data.Maybe (maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Hey.Components.Carousel (Axis(..), mkCarousel)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.Carousel.Controller (CarouselOptions, carouselProvider, defCarouselOptions, useCarouselController, useCarouselSlide)
import Hey.Pages.Landing (mkLandingPage)
import Hey.Pages.Repos (mkReposPage)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useMemo)
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
        ref /\ entry <- useCarouselSlide
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
  reposPage <- mkReposPage
  landingPage <- mkLandingPage
  pure
    $ [ { route: Home, component: landingPage }
      , { route: About, component: reposPage }
      ]

mkHomePage :: Component Env
mkHomePage = do
  let
    opts = defCarouselOptions { threshold = [ 0.0, 1.0 ] } :: CarouselOptions
  carousel <- mkCarousel
  routes <- mkRoutes
  threshold <- mkThreshold
  component "Home"
    $ \env -> React.do
        ref /\ value <- useCarouselController opts
        children <-
          useMemo unit \_ ->
            routes
              # map \{ route, component: c } ->
                  fragment [ threshold (env /\ route), c env ]
        pure
          $ carouselProvider
              { value
              , children:
                  pure
                    $ carousel
                        { ref
                        , axis: Y
                        , children
                        }
              }
