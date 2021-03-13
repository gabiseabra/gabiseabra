module Hey where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Menu (mkMenu)
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Background as Background
import Hey.Data.Env (Env, getOptions)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseFetch (mkFetchProvider)
import Hey.Hooks.UseRouter (useRouter)
import Hey.Hooks.UseScroller (mkScrollProvider, useScroller)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, readRefMaybe, useEffect, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Record.Extra (sequenceRecord)
import Web.DOM.Node as Node
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

mkBackground :: forall a. Component a
mkBackground =
  component "Background"
    $ \_ -> React.do
        bg /\ setBg <- useState Nothing
        scroller <- useScroller
        ref <- useRef null
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \node -> do
              x <- Background.mkCanvas
              setBg $ const $ Just x
              void $ Node.appendChild (Canvas.toNode x) node
              pure
                $ do
                    setBg $ const Nothing
                    Canvas.destroy x
        useEffect scroller
          $ maybe (mempty) (flip Background.setScroller $ scroller) bg
          *> pure mempty
        pure $ DOM.div { ref, id: "background" }

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
                  $ [ mempty -- menu env
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
