module Hey where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Menu (mkMenu)
import Hey.Env (Env, mkEnv)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import Hey.Router (Route(..))
import React.Basic.DOM (render)
import React.Basic.Hooks (Component, JSX, component, fragment)
import React.Basic.Hooks as React
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)
import Wire.React (useSignal)

mkRouter :: Component Env
mkRouter = do
  home <- mkHomePage
  notFound <- mkNotFoundPage
  component "Router" \env -> React.do
    route <- useSignal env.router.signal
    case route of
      NotFound -> pure $ notFound env
      _ -> pure $ home env

mkApp :: Effect JSX
mkApp = do
  eff /\ env <- mkEnv
  menu <- mkMenu
  router <- mkRouter
  pure $ fragment
    [ eff
    , menu env
    , router env
    ]

main :: Effect Unit
main = do
  app <- mkApp
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Container element not found."
    Just x  -> render app x