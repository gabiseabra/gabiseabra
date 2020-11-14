module Hey.Hooks.Carousel.Controller
  ( Axis(..)
  , CarouselOptions
  , CarouselController
  , CarouselController'
  , IntersectionObserverFn
  , defCarouselOptions
  , carouselProvider
  , UseCarouselController
  , useCarouselController
  , UseCarouselSlide
  , useCarouselSlide
  , UseCarouselContext
  , useCarouselContext
  , usePartialCarouselContext
  ) where

import Prelude

import Control.Applicative.Indexed (ipure)
import Control.Bind.Indexed ((:>>=))
import Data.Bifunctor (rmap)
import Data.Foldable (find)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, null)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Prim.Row as Row
import Prim.RowList as RL
import React.Basic.Hooks (Hook, JSX, ReactContext, Ref, UseContext, UseEffect, UseRef, UseState, coerceHook, component, contextProvider, createContext, element, readRefMaybe, useContext, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)
import React.Basic.Hooks.ResetToken (ResetToken, UseResetToken, useResetToken)
import Record.Extra (class Keys, pick)
import Web.DOM (Element, Node)
import Web.DOM.Element as Element
import Web.DOM.Node (isEqualNode)
import Web.IntersectionObserver (IntersectionObserver, disconnect, observe, unobserve)
import Web.IntersectionObserver as Observer
import Web.IntersectionObserverEntry (IntersectionObserverEntry)
import Web.IntersectionObserverEntry as ObserverEntry

data Axis = X | Y

type IntersectionObserverFn = Function (Array IntersectionObserverEntry -> IntersectionObserver)

type CarouselController' =
  ( axis :: Axis
  , observer :: IntersectionObserver
  , entries :: Array IntersectionObserverEntry
  , token :: ResetToken
  , slideTo :: String -> Effect Unit
  )

type CarouselController = {|CarouselController'}

type CarouselOptions =
  { axis :: Axis
  , rootMargin :: String
  , threshold :: Array Number
  , onChange :: Maybe (Array IntersectionObserverEntry -> IntersectionObserver -> Effect Unit)
  }

defCarouselOptions :: CarouselOptions
defCarouselOptions =
  { axis: X
  , rootMargin: "0px"
  , threshold: [0.0]
  , onChange: Nothing
  }

slideTo :: Element -> Axis -> String -> Effect Unit
slideTo el axis id = do
  pure unit

mkController :: CarouselOptions -> ResetToken -> Ref (Nullable Node) -> Effect (Maybe CarouselController)
mkController opts token ref =
  readRefMaybe ref >>= ((=<<) Element.fromNode) >>> case _ of
    Nothing -> pure Nothing
    Just el -> do
      let { axis, rootMargin, threshold } = opts
          onChange = fromMaybe (const $ const $ mempty) opts.onChange
      observer <- Observer.create onChange
        { root: Observer.Element el
        , rootMargin
        , threshold
        }
      pure $ Just
        { axis
        , observer
        , entries: mempty
        , token
        , slideTo: slideTo el axis
        }

ctx :: ReactContext (Maybe CarouselController)
ctx = unsafePerformEffect $ createContext Nothing

carouselProvider' :: { children :: Array JSX, value :: Maybe CarouselController } -> JSX
carouselProvider' = element $ contextProvider ctx

carouselProvider :: CarouselOptions -> (Ref (Nullable Node) -> Maybe CarouselController -> Array JSX) -> JSX
carouselProvider opts =
    unsafePerformEffect
  $ component "CarouselController"
  $ \mkChildren -> React.do
    ref /\ value <- useCarouselController opts
    pure $ carouselProvider'
      { value
      , children: mkChildren ref value
      }

type UseCarouselContext = UseContext (Maybe CarouselController)

useCarouselContext :: Hook UseCarouselContext (Maybe CarouselController)
useCarouselContext = useContext ctx

usePartialCarouselContext :: forall sr any rl
  .  Row.Union sr any CarouselController'
  => RL.RowToList sr rl
  => Keys rl
  => {|sr}
  -> Hook UseCarouselContext {|sr}
usePartialCarouselContext sr = useCarouselContext :>>= maybe sr pick >>> ipure

newtype UseCarouselController hooks
  = UseCarouselController
  (UseEffect Unit (UseState (Maybe CarouselController) (UseResetToken (UseRef (Nullable Node) hooks))))

derive instance newtypeUseCarouselController :: Newtype (UseCarouselController hooks) _

useCarouselController :: CarouselOptions -> Hook UseCarouselController (Ref (Nullable Node) /\ (Maybe CarouselController))
useCarouselController opts = coerceHook $ React.do
  nodeRef <- useRef null
  token /\ reset <- useResetToken
  state /\ setState <- useState Nothing
  let onChange entries _ = do
        setState $ maybe Nothing $ \st -> Just $ st { entries = entries }
        reset
      opts' = opts { onChange = Just onChange <> opts.onChange }
  useEffectOnce $ mkController opts' token nodeRef >>= case _ of
    Nothing -> pure mempty
    Just ctx -> do
      setState $ const $ Just ctx
      pure (disconnect ctx.observer)
  pure $ nodeRef /\  map (\st -> st { token = token }) state

newtype UseCarouselSlide hooks
  = UseCarouselSlide
  (UseAff ResetToken (Maybe IntersectionObserverEntry) (UseEffect Boolean (UseCarouselContext (UseResetToken (UseRef (Nullable Node) hooks)))))

derive instance newtypeUseCarouselSlide :: Newtype (UseCarouselSlide hooks) _

findEntry :: Array IntersectionObserverEntry -> Node -> Maybe IntersectionObserverEntry
findEntry entries node = 
  find
    ( ObserverEntry.target
    >>> Element.toNode
    >>> isEqualNode node
    >>> unsafePerformEffect
    ) entries

attachEntry :: IntersectionObserver -> Element -> Effect (Effect Unit)
attachEntry observer el = observe observer el *> pure (unobserve observer el)

useCarouselSlide :: Hook UseCarouselSlide (Ref (Nullable Node) /\ Maybe IntersectionObserverEntry)
useCarouselSlide = coerceHook $ React.do
  nodeRef <- useRef null
  defToken /\ _ <- useResetToken
  let
    useObserverEntry Nothing = React.do
      useEffect false $ pure mempty
      useAff defToken $ pure Nothing
    useObserverEntry (Just { observer, entries, token }) = React.do
      useEffect true
        $ readRefMaybe nodeRef
        >>= ((=<<) Element.fromNode)
        >>> maybe (pure mempty) (attachEntry observer)
      useAff token
        $ liftEffect
        $ readRefMaybe nodeRef
        >>= ((=<<) $ findEntry entries)
        >>> pure
  useCarouselContext
    :>>= useObserverEntry
    :>>= Tuple nodeRef
    >>> rmap join
    >>> pure
