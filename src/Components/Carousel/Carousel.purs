module Hey.Components.Carousel where

import Prelude

import Debug.Trace as Debug 

import Data.Maybe (maybe)
import Data.Nullable (Nullable)
import Data.String (length)
import Data.Tuple.Nested ((/\))
import Effect.Unsafe (unsafePerformEffect)
import Hey.Components.SVG.Definition (def)
import Hey.Components.SVG.Filters (anaglyph)
import Hey.Env (Env)
import Hey.Extra.Styles ((.&))
import Hey.Hooks.Carousel.Controller (Axis(..), carouselProvider_, defOptions, useCarouselContext, useCarouselSlide, usePartialCarouselContext)
import Hey.Router (Route(..), href)
import React.Basic (JSX, Ref)
import React.Basic.DOM (CSS)
import React.Basic.DOM as DOM
import React.Basic.DOM.SVG as SVG
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Web.DOM (Node)
import Web.IntersectionObserverEntry (IntersectionObserverEntry)
import Wire.React (useSignal)

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

type SlideProps =
  { render :: (IntersectionObserverEntry -> Array JSX)
  -- , style :: CSS
  , id :: String
  }

mkSlide :: Component SlideProps
mkSlide = component "Slide" $
  \{ render, id } -> React.do
    ref /\ entry <- useCarouselSlide
    pure $ DOM.div
      { ref
      , id
      -- , style
      , className: styles.slide
      , children: maybe [] render entry
      }

slide = unsafePerformEffect mkSlide :: SlideProps -> JSX

type CarouselProps =
  { ref :: Ref (Nullable Node)
  , children :: Array JSX
  }

mkCarousel :: Component CarouselProps
mkCarousel = component "Carousel" $
  \{ ref, children } -> React.do
    { axis } <- usePartialCarouselContext { axis: X }
    pure $ DOM.div
      { ref
      , className: styles.carousel .& axisCls axis
      , children
      }

carousel = unsafePerformEffect mkCarousel :: CarouselProps -> JSX
