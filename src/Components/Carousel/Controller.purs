module Hey.Hooks.Carousel.Controller
  ( CarouselOptions
  , IntersectionObserverFn
  , defCarouselOptions
  , carouselProvider
  , UseCarouselController
  , useCarouselController
  , UseCarouselSlide
  , useCarouselSlide
  , UseCarouselContext
  , useCarouselContext
  ) where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Bind.Indexed ((:>>=))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Hey.Extra.CustomEvent (customEvent, detail)
import Hey.Extra.FFI (unsafeInstanceCoerce)
import React.Basic.Hooks (Hook, JSX, ReactContext, Ref, UseContext, UseEffect, UseRef, UseState, coerceHook, contextProvider, createContext, element, readRefMaybe, useContext, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM (Element, Node)
import Web.DOM.Element (toEventTarget)
import Web.DOM.Element as Element
import Web.Event.CustomEvent (fromEvent, toEvent)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, dispatchEvent, eventListener, removeEventListener)
import Web.IntersectionObserver (IntersectionObserver, disconnect, observe, unobserve)
import Web.IntersectionObserver as Observer
import Web.IntersectionObserverEntry (IntersectionObserverEntry)
import Web.IntersectionObserverEntry as ObserverEntry

type IntersectionObserverFn
  = Function (Array IntersectionObserverEntry -> IntersectionObserver)

type CarouselOptions
  = { rootMargin :: String
    , threshold :: Array Number
    , onChange :: Maybe (Array IntersectionObserverEntry -> IntersectionObserver -> Effect Unit)
    }

defCarouselOptions :: CarouselOptions
defCarouselOptions =
  { rootMargin: "0px"
  , threshold: [ 0.0 ]
  , onChange: Nothing
  }

-- | Custom event which gets triggered on an intersection observer entry when it
-- | reaches a threshold.
intersectionChangeEvent = EventType "intersectionchange" :: EventType

notifyIntersectionChange :: IntersectionObserverEntry -> Effect Unit
notifyIntersectionChange entry = void $ dispatchEvent event target
  where
  target = toEventTarget $ ObserverEntry.target entry

  event = toEvent $ customEvent intersectionChangeEvent entry

mkController :: CarouselOptions -> Ref (Nullable Node) -> Effect (Maybe IntersectionObserver)
mkController opts ref =
  readRefMaybe ref >>= ((=<<) Element.fromNode)
    >>> case _ of
        Nothing -> pure Nothing
        Just el -> do
          let
            { rootMargin, threshold } = opts

            notify entries _ = fold $ map notifyIntersectionChange entries

            onChange = fromMaybe (const $ const $ mempty) opts.onChange <> notify
          observer <-
            Observer.create onChange
              { root: Observer.Element el
              , rootMargin
              , threshold
              }
          pure
            $ Just observer

ctx :: ReactContext (Maybe IntersectionObserver)
ctx = unsafePerformEffect $ createContext Nothing

carouselProvider :: { children :: Array JSX, value :: Maybe IntersectionObserver } -> JSX
carouselProvider = element $ contextProvider ctx

type UseCarouselContext
  = UseContext (Maybe IntersectionObserver)

useCarouselContext :: Hook UseCarouselContext (Maybe IntersectionObserver)
useCarouselContext = useContext ctx

newtype UseCarouselController hooks
  = UseCarouselController
  (UseEffect Unit (UseState (Maybe IntersectionObserver) (UseRef (Nullable Node) hooks)))

derive instance newtypeUseCarouselController :: Newtype (UseCarouselController hooks) _

useCarouselController :: CarouselOptions -> Hook UseCarouselController (Ref (Nullable Node) /\ (Maybe IntersectionObserver))
useCarouselController opts =
  coerceHook
    $ React.do
        nodeRef <- useRef null
        observer /\ setObserver <- useState Nothing
        useEffectOnce $ mkController opts nodeRef
          >>= case _ of
              Nothing -> pure mempty
              Just obs -> do
                setObserver $ const $ Just obs
                pure (disconnect obs)
        pure $ nodeRef /\ observer

newtype UseCarouselSlide hooks
  = UseCarouselSlide
  (UseEffect Boolean (UseCarouselContext (UseState (Maybe IntersectionObserverEntry) (UseRef (Nullable Node) hooks))))

derive instance newtypeUseCarouselSlide :: Newtype (UseCarouselSlide hooks) _

attachEntry :: IntersectionObserver -> Element -> Effect (Effect Unit)
attachEntry observer el = observe observer el *> pure (unobserve observer el)

attachIntersectionChangeListener :: (IntersectionObserverEntry -> Effect Unit) -> Element -> Effect (Effect Unit)
attachIntersectionChangeListener fun el = do
  let
    target = toEventTarget el
  listener <-
    eventListener
      $ (fromEvent >=> detail >>> unsafeInstanceCoerce "IntersectionObserverEntry")
      >>> maybe (pure unit) fun
  addEventListener intersectionChangeEvent listener false target
  pure $ removeEventListener intersectionChangeEvent listener false target

useCarouselSlide :: Hook UseCarouselSlide (Ref (Nullable Node) /\ Maybe IntersectionObserverEntry)
useCarouselSlide =
  coerceHook
    $ React.do
        nodeRef <- useRef null
        entry /\ setEntry <- useState Nothing
        let
          useObserverEntry Nothing = React.do
            useEffect false $ pure mempty

          useObserverEntry (Just observer) = React.do
            let
              attach = attachIntersectionChangeListener (Just >>> const >>> setEntry) <> attachEntry observer
            useEffect true
              $ readRefMaybe nodeRef
              >>= ((=<<) Element.fromNode)
              >>> maybe (pure mempty) attach
        useCarouselContext :>>= useObserverEntry
        ipure $ nodeRef /\ entry
