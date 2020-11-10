module Hey.Hooks.UseViewportSize
  ( useViewportSize
  , UseViewportSize
  , ViewportSize
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import React.Basic.Hooks (Hook, UseEffect, UseState, coerceHook, useEffectOnce, useState, (/\))
import React.Basic.Hooks as React
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.Window as Win

type ViewportSize =
  { width :: Int
  , height :: Int
  }

newtype UseViewportSize hooks
  = UseViewportSize
  (UseEffect Unit (UseEffect Unit (UseState ViewportSize hooks)))

derive instance newtypeUseViewportSize :: Newtype (UseViewportSize hooks) _

viewportSize :: Effect ViewportSize
viewportSize = do
  win <- window
  width <- Win.innerWidth win
  height <- Win.innerHeight win
  pure { width, height }

attachListener :: (ViewportSize -> Effect Unit) -> Effect (Effect Unit)
attachListener callback = do
  let etype = EventType "resize"
  target <- window >>= Win.toEventTarget >>> pure
  fun <- eventListener $ const $ viewportSize >>= callback
  addEventListener etype fun false target
  pure (removeEventListener etype fun false target)

useViewportSize :: Hook UseViewportSize ViewportSize
useViewportSize = coerceHook $ React.do
  size /\ setSize <- useState { width: 0, height: 0 }
  useEffectOnce $ viewportSize >>= const >>> setSize *> pure mempty
  useEffectOnce $ attachListener $ const >>> setSize
  pure size
