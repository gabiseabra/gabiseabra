module Hey.Hooks.UseScroll
  ( Key
  , mkScrollProvider
  , UseSnapPoint
  , useSnapPoint
  , useSnapPointRef
  , UseScrollerElement
  , useScrollerElement
  , offsetTop
  ) where

import Prelude
import Control.Monad.Indexed (ipure, (:>>=))
import Control.Plus (class Plus, empty)
import Data.Array (fromFoldable)
import Data.Foldable (class Foldable)
import Data.Map as Map
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, Ref, UseContext, UseEffect, UseState, coerceHook, component, contextProvider, createContext, element, readRefMaybe, useContext, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import React.Basic.Hooks.ResetToken (ResetToken, useResetToken)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (Element, Node)
import Web.DOM.Element as Element
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement

foreign import setSnapPoints :: Array (Effect (Array Number)) -> Effect Unit

foreign import updateScroller :: HTMLElement -> Effect Unit

foreign import offsetTop :: HTMLElement -> Effect Number

type Key
  = String

type SnapPoint
  = Effect (Array Number)

type ScrollContext
  = { token :: ResetToken
    , insert :: Key -> Effect (Array Number) -> Effect (Effect Unit)
    , scrollerRef :: Ref (Nullable Node)
    }

ctx :: ReactContext ScrollContext
ctx =
  unsafePerformEffect $ createContext
    $ { insert: \_ _ -> throw "useSnapPoint: Tried to add snap point with uninitialized context."
      , token: unsafeCoerce {}
      , scrollerRef: unsafeCoerce {}
      }

scrollProvider :: { children :: Array JSX, value :: ScrollContext } -> JSX
scrollProvider = element $ contextProvider ctx

mkScrollProvider :: Component (Array JSX)
mkScrollProvider =
  component "ScrollProvider"
    $ \children -> React.do
        scrollerRef <- useRef null
        token /\ reset <- useResetToken
        points /\ setPoints <- useState Map.empty
        useEffect token
          $ do
              readRefMaybe scrollerRef
                >>= ((=<<) HTMLElement.fromNode)
                >>> maybe mempty updateScroller
              setSnapPoints $ fromFoldable $ Map.values points
              pure mempty
        useEffectOnce $ pure $ setSnapPoints mempty
        let
          insert k point = do
            setPoints $ Map.insert k point
            reset
            pure $ setPoints $ Map.delete k
        pure
          $ scrollProvider
              { value: { token, insert, scrollerRef }
              , children:
                  [ DOM.div
                      { ref: scrollerRef
                      , style:
                          DOM.css
                            { position: "absolute"
                            , top: "0px"
                            , height: "100vh"
                            , width: "100vw"
                            , transformOrigin: "50% 50%"
                            , transformStyle: "preserve-3d"
                            }
                      }
                  , DOM.div_ children
                  ]
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

newtype UseScrollerElement hooks
  = ScrollerElement
  (UseAff Unit (Maybe Element) (UseContext ScrollContext hooks))

derive instance newtypeUseScrollerElement :: Newtype (UseScrollerElement hooks) _

useScrollerElement :: Hook UseScrollerElement (Maybe Element)
useScrollerElement =
  coerceHook $ useContext ctx
    :>>= (_.scrollerRef >>> readRefMaybe >=> ((=<<) Element.fromNode >>> pure))
    >>> liftEffect
    >>> useAff unit
    :>>= join
    >>> ipure
