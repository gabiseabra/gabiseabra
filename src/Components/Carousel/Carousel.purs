module Hey.Components.Carousel
  ( Axis(..)
  , SlideProps
  , RequiredSlideProps
  , OptionalSlideProps
  , mkSlide
  , CarouselProps
  , RequiredCarouselProps
  , OptionalCarouselProps
  , mkCarousel
  ) where

import Prelude

import Data.Maybe (Maybe)
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\))
import Hey.Extra.Props (PropsRep(..), runProps)
import Hey.Extra.Styles ((.&))
import Hey.Hooks.Carousel.Controller (useCarouselSlide)
import Prim.Row as Row
import React.Basic (JSX, Ref)
import React.Basic.DOM (CSS)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Web.DOM (Node)
import Web.IntersectionObserverEntry (IntersectionObserverEntry)

foreign import styles :: Styles

data Axis = X | Y

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
  ( className :: String
  , style :: CSS
  )

type SlideProps r = Record (RequiredSlideProps r)

slideProps = DefProps
  { className: ""
  , style: DOM.css {}
  } :: PropsRep (RequiredSlideProps ()) OptionalSlideProps

mkSlide :: forall opts opts'
  .  Row.Union opts opts' OptionalSlideProps
  => Component (SlideProps opts)
mkSlide = component "Slide" $ runProps slideProps >>>
  \{ render, id, className, style } -> React.do
    ref /\ entry <- useCarouselSlide
    pure $ DOM.div
      { ref
      , id
      , style
      , className: styles.slide .& className
      , children: render entry
      }

type RequiredCarouselProps r =
  ( ref :: Ref (Nullable Node)
  , children :: Array JSX
  , axis :: Axis
  | r
  )

type OptionalCarouselProps =
  ( className :: String
  , style :: CSS
  )

type CarouselProps r = Record (RequiredCarouselProps r) 

carouselProps = DefProps
  { className: ""
  , style: DOM.css {}
  } :: PropsRep (RequiredCarouselProps ()) OptionalCarouselProps

mkCarousel :: forall opts opts'
  .  Row.Union opts opts' OptionalCarouselProps
  => Component (CarouselProps opts)
mkCarousel = component "Carousel" $ runProps carouselProps >>>
  \{ axis, ref, children, className, style } -> React.do
    pure $ DOM.div
      { ref
      , style
      , className: styles.carousel .& axisCls axis .& className
      , children
      }
