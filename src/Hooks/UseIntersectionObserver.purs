module Hey.Hooks.UseIntersectionObserver
  ( IntersectionObserverOptions
  , IntersectionObserverFn
  , defOptions
  , mkIntersectionObserverProvider
  , UseIntersectionObserverEntry
  , useIntersectionObserverEntry
  , UseIntersectionObserverContext
  , useIntersectionObserverContext
  ) where

import Prelude
import Control.Applicative.Indexed (ipure)
import Control.Bind.Indexed ((:>>=))
import Data.Foldable (fold)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (Foreign)
import Hey.Extra.CustomEvent (customEvent, detail)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, Ref, UseContext, UseEffect, UseState, coerceHook, component, contextProvider, createContext, element, readRefMaybe, useContext, useEffect, useEffectOnce, useState)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node)
import Web.DOM.Element (toEventTarget)
import Web.DOM.Element as Element
import Web.Event.CustomEvent (fromEvent, toEvent)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, dispatchEvent, eventListener, removeEventListener)
import Web.IntersectionObserver (IntersectionObserver, IntersectionObserverInitRoot(..), disconnect, observe, unobserve)
import Web.IntersectionObserver as Observer
import Web.IntersectionObserverEntry (IntersectionObserverEntry)
import Web.IntersectionObserverEntry as ObserverEntry

type IntersectionObserverFn
  = Function (Array IntersectionObserverEntry -> IntersectionObserver)

type IntersectionObserverOptions
  = { root :: IntersectionObserverInitRoot
    , rootMargin :: String
    , threshold :: Array Number
    , onChange :: Maybe (Array IntersectionObserverEntry -> IntersectionObserver -> Effect Unit)
    }

defOptions :: IntersectionObserverOptions
defOptions =
  { root: None
  , rootMargin: "0px"
  , threshold: [ 0.0 ]
  , onChange: Nothing
  }

foreign import isIntersectionObserverEntry :: Foreign -> Boolean

coerceIntersectionObserverEntry :: Foreign -> Maybe IntersectionObserverEntry
coerceIntersectionObserverEntry a = if isIntersectionObserverEntry a then pure $ unsafeCoerce a else Nothing

-- | Custom event which gets triggered on an intersection observer entry when it
-- | reaches a threshold.
intersectionChangeEvent = EventType "intersectionchange" :: EventType

notifyIntersectionChange :: IntersectionObserverEntry -> Effect Unit
notifyIntersectionChange entry = void $ dispatchEvent event target
  where
  target = toEventTarget $ ObserverEntry.target entry

  event = toEvent $ customEvent intersectionChangeEvent entry

mkObserver :: IntersectionObserverOptions -> Effect (Maybe IntersectionObserver)
mkObserver opts@{ root, rootMargin, threshold } = do
  let
    notify entries _ = fold $ map notifyIntersectionChange entries

    onChange = fromMaybe (const $ const $ mempty) opts.onChange <> notify
  observer <-
    Observer.create onChange
      { root
      , rootMargin
      , threshold
      }
  pure $ Just observer

ctx :: ReactContext (Maybe IntersectionObserver)
ctx = unsafePerformEffect $ createContext Nothing

intersectionObserverProvider :: { children :: Array JSX, value :: Maybe IntersectionObserver } -> JSX
intersectionObserverProvider = element $ contextProvider ctx

mkIntersectionObserverProvider :: IntersectionObserverOptions -> Component (Array JSX)
mkIntersectionObserverProvider opts =
  component "IntersectionObserverProvider"
    $ \children -> React.do
        observer <- useIntersectionObserver opts
        pure $ intersectionObserverProvider { children, value: observer }

type UseIntersectionObserverContext
  = UseContext (Maybe IntersectionObserver)

useIntersectionObserverContext :: Hook UseIntersectionObserverContext (Maybe IntersectionObserver)
useIntersectionObserverContext = useContext ctx

newtype UseIntersectionObserver hooks
  = UseIntersectionObserver
  (UseEffect Unit (UseState (Maybe IntersectionObserver) hooks))

derive instance newtypeUseIntersectionObserver :: Newtype (UseIntersectionObserver hooks) _

useIntersectionObserver :: IntersectionObserverOptions -> Hook UseIntersectionObserver (Maybe IntersectionObserver)
useIntersectionObserver opts =
  coerceHook
    $ React.do
        observer /\ setObserver <- useState Nothing
        useEffectOnce
          $ mkObserver opts
          >>= case _ of
              Nothing -> pure mempty
              Just obs -> do
                setObserver $ const $ Just obs
                pure (disconnect obs)
        pure $ observer

newtype UseIntersectionObserverEntry hooks
  = UseIntersectionObserverEntry
  (UseEffect Boolean (UseIntersectionObserverContext (UseState (Maybe IntersectionObserverEntry) hooks)))

derive instance newtypeUseIntersectionObserverEntry :: Newtype (UseIntersectionObserverEntry hooks) _

attachEntry :: IntersectionObserver -> Element -> Effect (Effect Unit)
attachEntry observer el = observe observer el *> pure (unobserve observer el)

attachIntersectionChangeListener :: (IntersectionObserverEntry -> Effect Unit) -> Element -> Effect (Effect Unit)
attachIntersectionChangeListener fun el = do
  let
    target = toEventTarget el
  listener <-
    eventListener
      $ (fromEvent >=> detail >>> coerceIntersectionObserverEntry)
      >>> maybe (pure unit) fun
  addEventListener intersectionChangeEvent listener false target
  pure $ removeEventListener intersectionChangeEvent listener false target

useIntersectionObserverEntry :: Ref (Nullable Node) -> Hook UseIntersectionObserverEntry (Maybe IntersectionObserverEntry)
useIntersectionObserverEntry nodeRef =
  coerceHook
    $ React.do
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
        useIntersectionObserverContext :>>= useObserverEntry
        ipure $ entry
