module Hey.Hooks.UseFetch
  ( Hash
  , Fetch(..)
  , UseFetch
  , fetchProvider
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
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Exception (error, throw)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Component, Hook, JSX, ReactContext, UseContext, coerceHook, component, contextProvider, createContext, element, useContext, useState)
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (UseAff, useAff)

type Hash
  = String

data Fetch a
  = Fetch Hash (Request Json)

type FetchCache
  = Map Hash Json

type FetchContext
  = FetchCache /\ (String -> Json -> Effect Unit)

newtype UseFetch a hooks
  = UseFetch
  (UseAff Hash a (UseContext FetchContext hooks))

derive instance newtypeUseFetch :: Newtype (UseFetch a hooks) _

ctx :: ReactContext FetchContext
ctx = unsafePerformEffect $ createContext $ Map.empty /\ (\_ _ -> throw "useFetch: Tried to modify uninitialized fetch cache.")

fetchProvider :: { children :: Array JSX, value :: FetchContext } -> JSX
fetchProvider = element $ contextProvider ctx

mkFetchProvider :: Component (Array JSX)
mkFetchProvider =
  component "FetchProvider"
    $ \children -> React.do
        cache /\ setCache <- useState Map.empty
        pure $ fetchProvider { children, value: cache /\ \k v -> setCache $ Map.insert k v }

fetch :: forall a. Fetch a -> FetchCache -> Aff Json
fetch (Fetch hash req) cache = case Map.lookup hash cache of
  Just json -> pure json
  Nothing ->
    AX.request req
      >>= either (AX.printError >>> throw_) \{ body, status, statusText } ->
          if (unwrap status) `div` 100 == 2 then
            pure body
          else
            throwError $ error $ "Error " <> show status <> ": " <> statusText

throw_ :: forall m a. MonadThrow Error m => String -> m a
throw_ = error >>> throwError

useFetch :: forall a. DecodeJson a => Fetch a -> Hook (UseFetch a) (Maybe a)
useFetch req@(Fetch hash _) =
  coerceHook
    $ React.do
        cache /\ setRes <- useContext ctx
        useAff hash
          $ fetch req cache
          >>= (setRes hash *> (decodeJson >>> either (show >>> throw_) pure))
