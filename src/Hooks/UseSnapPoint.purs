module Hey.Hooks.UseSnapPoints
  ( Key
  , SnapPoint
  , UseSnapPoint
  , mkSnapPointProvider
  , useSnapPoint
  ) where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Indexed (ipure, (:>>=))
import Data.Array (fromFoldable)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, UseContext, UseEffect, coerceHook, component, contextProvider, createContext, element, useContext, useEffect, useEffectOnce, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.ResetToken (useResetToken)

foreign import setSnapPoints :: Array SnapPoint -> Effect Unit

type Key = String

type SnapPoint = Effect (Array Number)

type SnapPointContext
  = Key -> SnapPoint -> Effect (Effect Unit)

newtype UseSnapPoint hooks
  = UseFetch
  (UseEffect Key (UseContext SnapPointContext hooks))

derive instance newtypeUseSnapPoint :: Newtype (UseSnapPoint hooks) _

ctx :: ReactContext SnapPointContext
ctx = unsafePerformEffect $ createContext $ \_ _ -> throw "useSnapPoint: Tried to add snap point with uninitialized context."

snapPointProvider :: { children :: Array JSX, value :: SnapPointContext } -> JSX
snapPointProvider = element $ contextProvider ctx

mkSnapPointProvider :: Component (Array JSX)
mkSnapPointProvider =
  component "SnapPointProvider"
    $ \children -> React.do
        token /\ reset <- useResetToken
        points /\ setPoints <- useState Map.empty
        useEffect token $ do
          setSnapPoints $ fromFoldable $ Map.values points
          pure mempty
        useEffectOnce $ pure $ setSnapPoints mempty
        let update k point = do
              setPoints $ Map.insert k point
              reset
              pure $ setPoints $ Map.delete k
        pure $ snapPointProvider { children, value: update }

useSnapPoint :: Key -> SnapPoint -> Hook UseSnapPoint Unit
useSnapPoint k p = coerceHook $ useContext ctx :>>= slip lift2 (ipure k) (ipure p) :>>= useEffect k
  where slip fun a b c = fun c a b
