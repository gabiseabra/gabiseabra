module Hey where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Canvas.Background as Background
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
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument as Doc
import Web.HTML.HTMLElement as El
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
  doc <- Win.document =<< window
  body <- Doc.body doc
  root <- getElementById "root" $ Doc.toNonElementParentNode doc
  case body of
    Nothing -> throw "Body element not found."
    Just x -> void $ join $ appendChild <$> Background.mkCanvas <*> (pure $ El.toNode x)
  case root of
    Nothing -> throw "Container element not found."
    Just x -> render (app {}) x
