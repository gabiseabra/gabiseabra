module Hey where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Menu (mkMenu)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseFetch (mkFetchProvider)
import Hey.Hooks.UseRouter (useRouter)
import Hey.Hooks.UseScroll (mkScrollProvider)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import React.Basic.DOM (render)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component)
import React.Basic.Hooks as React
import Record.Extra (sequenceRecord)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
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
  routes <- mkRoutes
  fetchProvider <- mkFetchProvider
  scrollProvider <- mkScrollProvider
  component "App"
    $ \_ -> React.do
        router <- useRouter
        sequenceRecord { router }
          # case _ of
              Nothing -> pure mempty
              Just env ->
                fetchProvider
                  >:> scrollProvider
                  >>> pure
                  $ [ menu env
                    , DOM.main_ [ routes env ]
                    ]

main :: Effect Unit
main = do
  app <- mkApp
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Container element not found."
    Just x -> render (app {}) x
