module Hey.Pages.Home
  ( mkHomePage
  ) 
  where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Hey.Components.Carousel (Axis(..), mkCarousel, mkSlide)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.Carousel.Controller (CarouselOptions, carouselProvider, defCarouselOptions, useCarouselController)
import Hey.Pages.Landing (mkLandingPage)
import React.Basic (JSX)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useEffect, useMemo)
import React.Basic.Hooks as React
import Record (disjointUnion)
import Type.Row (type (+))
import Web.IntersectionObserverEntry (IntersectionObserverEntry, intersectionRatio)
import Wire.React (useSignal)

opts = defCarouselOptions { threshold = [0.9] } :: CarouselOptions

type EnvProps r = ( env :: Env | r )
type RouteProps r = ( route :: Route , component :: Env -> JSX | r )
type IntersectionEntryProps r = ( intersectionRatio :: Number | r )
type PageProps = RouteProps + IntersectionEntryProps + EnvProps + ()

mkPage :: Component {|PageProps}
mkPage = component "Landing" $
  \{ intersectionRatio, route, component, env } -> React.do
    -- | update page hash whenever a page becomes visible
    currentRoute <- useSignal env.router.signal
    useEffect intersectionRatio $ do
      if intersectionRatio >= 0.9 && route /= currentRoute
      then env.router.replace route
      else pure unit
      pure mempty
    pure $ component env

mkRoutes :: Effect (Array {|RouteProps ()})
mkRoutes = do
  let aboutPage _ = DOM.text "lmaoo"
  landingPage <- mkLandingPage
  pure $
    [ { route: Home, component: landingPage }
    , { route: About, component: aboutPage }
    ]

intersectionEntryProps :: Maybe IntersectionObserverEntry -> {|IntersectionEntryProps ()}
intersectionEntryProps Nothing = { intersectionRatio: 0.0 }
intersectionEntryProps (Just entry) = { intersectionRatio: intersectionRatio entry }

mkHomePage :: Component Env
mkHomePage = do
  carousel <- mkCarousel
  slide <- mkSlide
  routes <- mkRoutes
  page <- mkPage
  component "Home" $ \env -> React.do
    ref /\ value <- useCarouselController opts
    children <- useMemo unit \_ ->
      routes # map \{ route, component } ->
        slide
          { id: show route
          , render:
              intersectionEntryProps
              >>> disjointUnion { route, component, env }
              >>> page
              >>> pure
          }
    pure $ carouselProvider
      { value
      , children: pure $ carousel
        { ref
        , axis: Y
        , children
        }
      }
