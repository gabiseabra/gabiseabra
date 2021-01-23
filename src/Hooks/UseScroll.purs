module Hey.Hooks.UseScroll
  ( ScrollTrigger
  , ScrollTriggerOptions
  , snapTo
  , mkScrollProvider
  , UseSnapPoint
  , useSnapPoint
  , useScrollTrigger
  ) where

import Prelude
import Data.Array (fromFoldable)
import Data.HashMap as HMap
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, Ref, UseContext, UseEffect, coerceHook, component, contextProvider, createContext, element, readRefMaybe, useContext, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.ResetToken (useResetToken)
import Web.DOM (Node)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

type ScrollTriggerOptions
  = { onEnter :: ScrollTrigger -> Effect Unit
    , onEnterBack :: ScrollTrigger -> Effect Unit
    }

foreign import data ScrollTrigger :: Type

foreign import mkIdx :: Effect Int

foreign import setSnapPoints :: Array HTMLElement -> Effect Unit

foreign import mkScrollTrigger :: ScrollTriggerOptions -> HTMLElement -> Effect ScrollTrigger

foreign import kill :: ScrollTrigger -> Effect Unit

-- | Scroll to the snap point closest to the element
foreign import snapTo :: HTMLElement -> Effect Unit

type ScrollContext
  = { insert :: HTMLElement -> Effect (Effect Unit)
    }

ctx :: ReactContext ScrollContext
ctx =
  unsafePerformEffect
    $ createContext
        { insert: \_ -> throw "useSnapPoint: Tried to add snap point with uninitialized context."
        }

scrollProvider :: { children :: Array JSX, value :: ScrollContext } -> JSX
scrollProvider = element $ contextProvider ctx

mkScrollProvider :: Component (Array JSX)
mkScrollProvider =
  component "ScrollProvider"
    $ \children -> React.do
        scrollerRef <- useRef null
        token /\ reset <- useResetToken
        points /\ setPoints <- useState HMap.empty
        useEffect token
          $ do
              setSnapPoints $ fromFoldable $ HMap.values points
              pure mempty
        useEffectOnce $ pure $ setSnapPoints mempty
        let
          insert el = do
            idx <- mkIdx
            setPoints $ HMap.insert idx el
            reset
            pure $ setPoints $ HMap.delete idx
        pure
          $ scrollProvider
              { value: { insert }
              , children
              }

newtype UseSnapPoint hooks
  = UseSnapPoint
  (UseEffect Unit (UseContext ScrollContext hooks))

derive instance newtypeUseSnapPoint :: Newtype (UseSnapPoint hooks) _

useSnapPoint :: Ref (Nullable Node) -> Hook UseSnapPoint Unit
useSnapPoint ref =
  coerceHook
    $ React.do
        { insert } <- useContext ctx
        useEffectOnce $ readRefMaybe ref
          >>= ((=<<) HTMLElement.fromNode)
          >>> maybe (pure mempty) insert

useScrollTrigger :: forall a. Eq a => a -> Ref (Nullable Node) -> ScrollTriggerOptions -> Hook (UseEffect a) Unit
useScrollTrigger deps ref o =
  useEffect deps
    $ readRefMaybe ref
    >>= ((=<<) HTMLElement.fromNode)
    >>> maybe (pure mempty) (mkScrollTrigger o >=> kill >>> pure)
