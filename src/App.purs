module Hey.App where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Hey.Components.Menu (mkMenu)
import Hey.Env (Env, mkEnv)
import Hey.Pages.Home (mkHomePage)
import Hey.Pages.NotFound (mkNotFoundPage)
import Hey.Router (Route(..))
import React.Basic.Hooks (Component, JSX, component, fragment)
import React.Basic.Hooks as React
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
