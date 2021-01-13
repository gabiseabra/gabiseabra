module Hey.Hooks.UseSnapPoints
  ( Key
  , mkScrollProvider
  , useScrollHeight
  , UseSnapPoint
  , useSnapPoint
  ) where

import Prelude
import Control.Monad.Indexed (ipure, (:>>=))
import Control.Plus (class Plus, empty)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Newtype (class Newtype)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, UseContext, UseEffect, UseState, coerceHook, component, contextProvider, createContext, element, useContext, useEffect, useEffectOnce, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.ResetToken (useResetToken)

foreign import setSnapPoints :: Array (Effect (Array Number)) -> Effect Unit

foreign import getScrollHeight :: Effect Number

type Key
  = String

type SnapPoint
  = Effect (Array Number)

type ScrollContext
  = { insert :: Key -> Effect (Array Number) -> Effect (Effect Unit)
    , scrollHeight :: Number
    }

newtype UseSnapPoint f hooks
  = UseSnapPoint
  (UseEffect Key (UseContext ScrollContext (UseState (f Number) hooks)))

derive instance newtypeUseSnapPoint :: Newtype (UseSnapPoint f hooks) _

ctx :: ReactContext ScrollContext
ctx =
  unsafePerformEffect $ createContext
    $ { insert: \_ _ -> throw "useSnapPoint: Tried to add snap point with uninitialized context."
      , scrollHeight: 0.0
      }

snapPointProvider :: { children :: Array JSX, value :: ScrollContext } -> JSX
snapPointProvider = element $ contextProvider ctx

mkScrollProvider :: Component (Array JSX)
mkScrollProvider =
  component "SnapPointProvider"
    $ \children -> React.do
        token /\ reset <- useResetToken
        points /\ setPoints <- useState Map.empty
        scrollHeight /\ setScrollHeight <- useState 0.0
        useEffect token
          $ do
              setSnapPoints $ fromFoldable $ Map.values points
              getScrollHeight >>= const >>> setScrollHeight
              pure mempty
        useEffectOnce $ pure $ setSnapPoints mempty
        let
          insert k point = do
            setPoints $ Map.insert k point
            reset
            pure $ setPoints $ Map.delete k
        pure $ snapPointProvider { children, value: { insert, scrollHeight } }

useSnapPoint :: forall f. Foldable f => Plus f => Key -> Effect (f Number) -> Hook (UseSnapPoint f) (f Number)
useSnapPoint k p =
  coerceHook
    $ React.do
        state /\ setState <- useState empty
        points <- useContext ctx
        useEffect k $ points.insert k
          $ p
          >>= (const >>> setState)
          *> (fromFoldable >>> pure)
        pure state

useScrollHeight :: Hook (UseContext ScrollContext) Number
useScrollHeight = useContext ctx :>>= _.scrollHeight >>> ipure
