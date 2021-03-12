module Hey.Hooks.UseScroller
  ( Scroller
  , UseScrollTrigger
  , mkScrollProvider
  , useScroller
  , useScrollTrigger
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, Ref, UseContext, UseEffect, coerceHook, component, contextProvider, createContext, element, fragment, readRefMaybe, useContext, useEffectOnce, useState)
import React.Basic.Hooks as React
import Web.DOM (Node)

foreign import data Scroller :: Type

foreign import nullScroller :: Scroller

foreign import mkScroller :: Node -> Effect Scroller

foreign import mkScrollTrigger :: Scroller -> Effect Unit -> Node -> Effect (Effect Unit)

foreign import destroy :: Scroller -> Effect Unit

ctx :: ReactContext Scroller
ctx =
  unsafePerformEffect
    $ createContext nullScroller

scrollProvider :: { children :: Array JSX, value :: Scroller } -> JSX
scrollProvider = element $ contextProvider ctx

mkScrollProvider :: Component (Ref (Nullable Node) /\ Array JSX)
mkScrollProvider =
  component "ScrollProvider"
    $ \(ref /\ children) -> React.do
        scroller /\ setScroller <- useState Nothing
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \node -> do
              s <- mkScroller node
              setScroller $ const $ Just s
              pure $ destroy s
        pure
          $ scroller
          # maybe (fragment children) \s ->
              scrollProvider
                { value: s
                , children
                }

useScroller :: Hook (UseContext Scroller) Scroller
useScroller = useContext ctx

newtype UseScrollTrigger hooks
  = UseScrollTrigger
  (UseEffect Unit (UseContext Scroller hooks))

derive instance netypeUseScrollTrigger :: Newtype (UseScrollTrigger hooks) _

useScrollTrigger :: Ref (Nullable Node) -> Effect Unit -> Hook UseScrollTrigger Unit
useScrollTrigger ref f =
  coerceHook
    $ React.do
        scroller <- useScroller
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) (mkScrollTrigger scroller f)
