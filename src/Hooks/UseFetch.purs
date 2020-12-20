module Hey.Hooks.UseFetch
  ( Hash
  , Fetch(..)
  , UseFetch
  , mkFetchProvider
  , useFetch
  ) where

import Prelude
import Affjax (Request)
import Affjax as AX
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Either (either)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Exception (error, throw)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, UseContext, UseEffect, coerceHook, component, contextProvider, createContext, element, useContext, useEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, mkAffReducer, noEffects, useAff, useAffReducer)

type Hash
  = String

data Fetch a
  = Fetch Hash (Request Json)

data Action
  = FetchA Hash (Request Json)
  | PutA Hash Json

type FetchCache
  = Map Hash Json

type FetchContext
  = FetchCache /\ (Hash /\ Request Json -> Effect Unit)

newtype UseFetch a hooks
  = UseFetch
  (UseAff (Maybe Json) (Maybe a) (UseEffect Hash (UseContext FetchContext hooks)))

derive instance newtypeUseFetch :: Newtype (UseFetch a hooks) _

fetch :: Hash /\ Request Json -> Aff Json
fetch (hash /\ req) =
  AX.request req
    >>= either (AX.printError >>> throw_) \{ body, status, statusText } ->
        if (unwrap status) `div` 100 == 2 then
          pure body
        else
          throwError $ error $ "Error " <> show status <> ": " <> statusText

throw_ :: forall m a. MonadThrow Error m => String -> m a
throw_ = error >>> throwError

fetchReducer :: FetchCache -> Action -> { state :: FetchCache, effects :: Array (Aff (Array Action)) }
fetchReducer cache (PutA k r) = noEffects $ Map.insert k r cache

fetchReducer cache (FetchA k r) = case Map.lookup k cache of
  Just _ -> noEffects cache
  Nothing ->
    { state: cache
    , effects: pure $ map (PutA k >>> pure) (fetch (k /\ r))
    }

ctx :: ReactContext FetchContext
ctx = unsafePerformEffect $ createContext $ Map.empty /\ (\_ -> throw "useFetch: Tried to modify uninitialized fetch cache.")

fetchProvider :: { children :: Array JSX, value :: FetchContext } -> JSX
fetchProvider = element $ contextProvider ctx

mkFetchProvider :: Component (Array JSX)
mkFetchProvider = do
  reducer <- mkAffReducer $ fetchReducer
  component "FetchProvider"
    $ \children -> React.do
        cache /\ enqueue <- useAffReducer Map.empty reducer
        pure
          $ fetchProvider
              { children
              , value: cache /\ \(k /\ r) -> enqueue (FetchA k r)
              }

useFetch :: forall a. DecodeJson a => Fetch a -> Hook (UseFetch a) (Maybe a)
useFetch req@(Fetch k r) =
  coerceHook
    $ React.do
        cache /\ enqueue <- useContext ctx
        let
          res = Map.lookup k cache

          decode' :: Json -> Aff a
          decode' = decodeJson >>> either (show >>> throw_) pure
        useEffect k
          $ do
              enqueue (k /\ r)
              pure mempty
        decoded <-
          useAff res
            $ maybe (pure Nothing) (decode' >>> map Just) res
        pure $ join decoded
