module Hey.Pages.Home
  ( mkHomePage
  ) where

import Prelude
import Data.Nullable (null)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseScroll (useScrollTrigger)
import Hey.Pages.About (mkAboutPage)
import Hey.Pages.Github (mkGithubPage)
import Hey.Pages.Spacer (mkSpacerPage)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { container :: String
    , page :: String
    , threshold :: String
    }

type PageProps
  = { env :: Env
    , route :: Route
    , children :: Array JSX
    }

spacer :: JSX
spacer = DOM.div { style: DOM.css { width: "100vw", height: "100vh" } }

mkPage :: Component ({ env :: Env, route :: Route, children :: Array JSX })
mkPage =
  component "Page"
    $ \{ env, route, children } -> React.do
        let
          updateRoute = const $ env.router.replace route
        ref <- useRef null
        useScrollTrigger unit ref updateRoute
        pure
          $ DOM.div
              { ref
              , id: show route
              , className: styles.page
              , children
              }

mkRoutes :: Effect (Array (Route /\ (Env -> JSX)))
mkRoutes = do
  spacerPage <- mkSpacerPage
  aboutPage <- mkAboutPage
  githubPage <- mkGithubPage
  pure
    $ [ Home /\ spacerPage
      , About /\ aboutPage
      , Projects /\ githubPage
      , End /\ spacerPage
      ]

mkHomePage :: Component Env
mkHomePage = do
  routes <- mkRoutes
  page <- mkPage
  component "Home"
    $ \env ->
        pure $ fragment $ routes
          <#> \(route /\ c) ->
              page
                { env
                , route
                , children: [ c env ]
                }
