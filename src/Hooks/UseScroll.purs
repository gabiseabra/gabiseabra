module Hey.Hooks.UseScroll
  ( Key
  , mkScrollProvider
  , useScrollHeight
  , UseSnapPoint
  , useSnapPoint
  , useSnapPointRef
  , UsePerspective
  , usePerspective
  , offsetTop
  ) where

import Prelude
import Control.Monad.Indexed (ipure, (:>>=))
import Control.Plus (class Plus, empty)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Debug.Trace as Debug
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Math as Math
import React.Basic.DOM (CSS)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, Ref, UseContext, UseEffect, UseState, coerceHook, component, contextProvider, createContext, element, readRefMaybe, useContext, useEffect, useEffectOnce, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import React.Basic.Hooks.ResetToken (ResetToken, useResetToken)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Node)
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

foreign import setSnapPoints :: Array (Effect (Array Number)) -> Effect Unit

foreign import getScrollHeight :: Effect Number

foreign import updateScroller :: Number -> Effect Unit

foreign import scrollAngle :: Number

foreign import offsetTop :: HTMLElement -> Effect Number

type Key
  = String

type SnapPoint
  = Effect (Array Number)

type ScrollContext
  = { token :: ResetToken
    , insert :: Key -> Effect (Array Number) -> Effect (Effect Unit)
    , scrollHeight :: Number
    }

ctx :: ReactContext ScrollContext
ctx =
  unsafePerformEffect $ createContext
    $ { insert: \_ _ -> throw "useSnapPoint: Tried to add snap point with uninitialized context."
      , scrollHeight: 0.0
      , token: unsafeCoerce {}
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
              getScrollHeight >>= updateScroller <> (const >>> setScrollHeight)
              pure mempty
        useEffectOnce $ pure $ setSnapPoints mempty
        let
          insert k point = do
            setPoints $ Map.insert k point
            reset
            pure $ setPoints $ Map.delete k
        pure
          $ snapPointProvider
              { children
              , value: { token, insert, scrollHeight }
              }

newtype UseSnapPoint f hooks
  = UseSnapPoint
  (UseEffect Key (UseContext ScrollContext (UseState (f Number) hooks)))

derive instance newtypeUseSnapPoint :: Newtype (UseSnapPoint f hooks) _

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

useSnapPointRef :: Key -> Ref (Nullable Node) -> Hook (UseSnapPoint Maybe) (Maybe Number)
useSnapPointRef k ref =
  useSnapPoint k
    $ readRefMaybe ref
    >>= ((=<<) HTMLElement.fromNode)
    >>> traverse offsetTop

useScrollHeight :: Hook (UseContext ScrollContext) Number
useScrollHeight = useContext ctx :>>= _.scrollHeight >>> ipure

newtype UsePerspective hooks
  = UsePerspective
  (UseAff Number (Maybe CSS) (UseContext ScrollContext hooks))

derive instance newtypeUsePerspective :: Newtype (UsePerspective hooks) _

perspectiveCSS :: Number -> HTMLElement -> Effect CSS
perspectiveCSS scrollHeight el = do
  offset <- offsetTop el
  let
    y = (offset `div` scrollHeight)

    angle = y * scrollAngle - (Math.pi * 0.5)

    z = scrollHeight `div` scrollAngle
  Debug.traceM { el, scrollHeight, y, z }
  pure
    $ DOM.css
        { transform: "translate3d(0,0,-" <> (show z) <> "px) rotateX(" <> (show angle) <> "rad)"
        , transformOrigin: "50% 0%"
        }

usePerspective :: Ref (Nullable Node) -> Hook UsePerspective CSS
usePerspective ref =
  coerceHook
    $ React.do
        { token, scrollHeight } <- useContext ctx
        css <-
          useAff scrollHeight
            $ liftEffect
            $ readRefMaybe ref
            >>= ((=<<) HTMLElement.fromNode >>> pure)
            >>= traverse (perspectiveCSS scrollHeight)
        pure $ fromMaybe (DOM.css {}) $ join css
