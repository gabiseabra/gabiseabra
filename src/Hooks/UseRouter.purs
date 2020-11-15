module Hey.Hooks.UseRouter
  ( UseRouter
  , useRouter
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Foreign (unsafeToForeign)
import Hey.Data.Env (Router)
import Hey.Data.Route (match, href)
import React.Basic.Hooks (Hook, UseEffect, UseState, coerceHook, useEffectOnce, useState)
import React.Basic.Hooks as React
import Routing.Hash as Hash
import Routing.PushState as PushState
import Wire.Signal as Signal

newtype UseRouter hooks
  = UseRouter
  (UseEffect Unit (UseState (Maybe Router) hooks))

derive instance newtypeUseRouter :: Newtype (UseRouter hooks) _

useRouter :: Hook UseRouter (Maybe Router)
useRouter = coerceHook $ React.do
  router /\ setRouter <- useState Nothing
  useEffectOnce $ do
    { signal, modify } <- Hash.getHash >>= match >>> Signal.create
    { listen, pushState, replaceState, locationState } <- PushState.makeInterface
    { pathname } <- locationState
    setRouter $ const $ Just
      { signal
      , push: stateFn pushState
      , replace: stateFn replaceState
      }
    modify $ const $ match pathname
    listen (_.pathname >>> match >>> const >>> modify)
  pure router
  where stateFn fun = fun (unsafeToForeign {}) <<< href
