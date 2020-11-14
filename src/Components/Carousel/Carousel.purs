module Hey.Components.Carousel where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Hey.Extra.Props (PropsRep(..), runProps)
import Hey.Extra.Styles ((.&))
import Hey.Hooks.Carousel.Controller (Axis(..), useCarouselSlide, usePartialCarouselContext)
import Prim.Row as Row
import React.Basic (JSX, Ref)
import React.Basic.DOM (CSS)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Web.DOM (Node)
import Web.IntersectionObserverEntry (IntersectionObserverEntry)

foreign import styles :: Styles

axisCls :: Axis -> String
axisCls X = styles.xAxis
axisCls Y = styles.yAxis

type Styles =
  { carousel :: String
  , xAxis :: String
  , yAxis :: String
  , slide :: String
  }

type RequiredSlideProps r =
  ( render :: Maybe IntersectionObserverEntry -> Array JSX
  , id :: String
  | r
  )

type OptionalSlideProps =
  ( style :: CSS
  , className :: String
  )

type SlideProps r = Record (RequiredSlideProps r)
sliderProps = DefProps
  { className: ""
  , style: DOM.css {}
  } :: PropsRep (RequiredSlideProps ()) OptionalSlideProps

mkSlide :: forall opts opts'
  .  Row.Union opts opts' OptionalSlideProps
  => Component (SlideProps opts)
mkSlide = component "Slide" $ runProps sliderProps >>>
  \{ render, id } -> React.do
    ref /\ entry <- useCarouselSlide
    pure $ DOM.div
      { ref
      , id
      -- , style
      , className: styles.slide
      , children: render entry
      }

slide :: forall opts opts'
  .  Row.Union opts opts' OptionalSlideProps
  => SlideProps opts
  -> JSX
slide = unsafePerformEffect mkSlide

type RequiredCarouselProps r =
  ( ref :: Ref (Nullable Node)
  , children :: Array JSX
  | r
  )

type OptionalCarouselProps =
  ( style :: CSS
  )

type CarouselProps r = Record (RequiredCarouselProps r) 

carouselProps = DefProps
  { style: DOM.css {}
  } :: PropsRep (RequiredCarouselProps ()) OptionalCarouselProps

mkCarousel :: forall opts opts'
  .  Row.Union opts opts' OptionalCarouselProps
  => Component (CarouselProps opts)
mkCarousel = component "Carousel" $ runProps carouselProps >>>
  \{ ref, children, style } -> React.do
    { axis } <- usePartialCarouselContext { axis: X }
    pure $ DOM.div
      { ref
      , style
      , className: styles.carousel .& axisCls axis
      , children
      }

carousel :: forall opts opts'
  .  Row.Union opts opts' OptionalCarouselProps
  => CarouselProps opts
  -> JSX
carousel = unsafePerformEffect mkCarousel
