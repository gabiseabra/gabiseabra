module Hey where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Components.Menu (menu)
import Hey.Pages.Home (mkHomePage)
import React.Basic.DOM (render)
import React.Basic.Hooks (Component, fragment)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

mkApp :: Component {}
mkApp = do
  home <- mkHomePage
  pure $ \_ ->
    fragment
    [ menu
    , home {}
    ]

main :: Effect Unit
main = do
  app <- mkApp
  root <- getElementById "root" =<< (map toNonElementParentNode $ document =<< window)
  case root of
    Nothing -> throw "Container element not found."
    Just x  -> render (app {}) x