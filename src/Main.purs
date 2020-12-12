module Hey where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Menu (mkMenu)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseFetch (mkFetchProvider)
import Hey.Hooks.UseIntersectionObserver (IntersectionObserverOptions, intersectionObserverProvider, useIntersectionObserver)
import Hey.Hooks.UseIntersectionObserver as UseIntersectionObserver
import Hey.Hooks.UseRouter (useRouter)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import React.Basic.DOM (render)
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React
import Record.Extra (sequenceRecord)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body, toNonElementParentNode)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)
import Web.IntersectionObserver as IntersectionObserver
import Wire.React (useSignal)

mkRoutes :: Component Env
mkRoutes = do
  home <- mkHomePage
  notFound <- mkNotFoundPage
  component "Router" \env -> React.do
    route <- useSignal env.router.signal
    case route of
      NotFound -> pure $ notFound env
      _ -> pure $ home env

intersectionOpts :: Effect IntersectionObserverOptions
intersectionOpts = do
  root <-
    window
      >>= document
      >>= body
      >>= maybe IntersectionObserver.None (toElement >>> IntersectionObserver.Element)
      >>> pure
  pure
    $ UseIntersectionObserver.defOptions
        { root = root
        , threshold = [ 0.0, 1.0 ]
        }

mkApp :: forall a. Component a
mkApp = do
  menu <- mkMenu
  routes <- mkRoutes
  fetchProvider <- mkFetchProvider
  component "App"
    $ \_ -> React.do
        observer <- useIntersectionObserver intersectionOpts
        router <- useRouter
        sequenceRecord { router }
          # case _ of
              Nothing -> pure mempty
              Just env ->
                pure
                  $ intersectionObserverProvider
                      { value: observer
                      , children:
                          pure
                            $ fetchProvider
                                [ menu env
                                , routes env
                                ]
                      }

main :: Effect Unit
main = do
  app <- mkApp
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Container element not found."
    Just x -> render (app {}) x
