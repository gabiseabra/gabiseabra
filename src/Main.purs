module Hey where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Menu (mkMenu)
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Background as Background
import Hey.Data.Env (Env, getOptions)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseFetch (mkFetchProvider)
import Hey.Hooks.UseRouter (useRouter)
import Hey.Hooks.UseScroll (mkScrollProvider)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component)
import React.Basic.Hooks as React
import Record.Extra (sequenceRecord)
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode as NEPN
import Web.HTML (HTMLElement)
import Web.HTML as HTML
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
  options <- getOptions
  component "App"
    $ \_ -> React.do
        router <- useRouter
        sequenceRecord { router, options: Just options }
          # case _ of
              Nothing -> pure mempty
              Just env ->
                fetchProvider
                  >:> scrollProvider
                  >>> pure
                  $ [ menu env
                    , DOM.main_ [ routes env ]
                    ]

mkBackground :: HTMLElement -> Effect Unit
mkBackground body =
  void
    $ join
    $ Node.appendChild
    <$> (Canvas.toNode <$> Background.mkCanvas)
    <*> (pure $ El.toNode body)

main :: Effect Unit
main = do
  app <- mkApp
  doc <- Win.document =<< HTML.window
  body <- Doc.body doc
  root <- NEPN.getElementById "root" $ Doc.toNonElementParentNode doc
  case body of
    Nothing -> throw "Body element not found."
    Just x -> mkBackground x
  case root of
    Nothing -> throw "Container element not found."
    Just x -> DOM.render (app {}) x
