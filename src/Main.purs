module Hey where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Background (mkBackground)
import Hey.Components.Menu (mkMenu)
import Hey.Data.Env (Env, getOptions)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseFetch (mkFetchProvider)
import Hey.Hooks.UseRouter (useRouter)
import Hey.Hooks.UseScroller (mkScrollProvider)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, useRef, useState)
import React.Basic.Hooks as React
import Record.Extra (sequenceRecord)
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (HTMLDocument)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as Doc
import Web.HTML.Window as Win
import Wire.React (useSignal)

composeProvider :: (Array JSX -> JSX) -> (Array JSX -> JSX) -> Array JSX -> JSX
composeProvider f g = f >>> pure >>> g

infixr 10 composeProvider as >:>

mkRoutes :: Component Env
mkRoutes = do
  home <- mkHomePage
  notFound <- mkNotFoundPage
  component "Router" \env -> React.do
    route <- useSignal env.router.signal
    case route of
      NotFound -> pure $ notFound env
      _ -> pure $ home env

mkApp :: forall a. Component a
mkApp = do
  menu <- mkMenu
  bg <- mkBackground
  routes <- mkRoutes
  fetchProvider <- mkFetchProvider
  scrollProvider <- mkScrollProvider
  options <- getOptions
  component "App"
    $ \_ -> React.do
        ref <- useRef null
        router <- useRouter
        sequenceRecord { router, options: Just options }
          # case _ of
              Nothing -> pure mempty
              Just env ->
                fetchProvider
                  >:> ((/\) ref >>> scrollProvider)
                  >>> pure
                  $ [ menu env
                    , bg {}
                    , DOM.main { ref, children: [ routes env ] }
                    ]

doc :: Effect HTMLDocument
doc = Win.document =<< HTML.window

main :: Effect Unit
main = do
  app <- mkApp
  root <- NEPN.getElementById "root" <<< Doc.toNonElementParentNode =<< doc
  case root of
    Nothing -> throw "Container element not found."
    Just x -> DOM.render (app {}) x
