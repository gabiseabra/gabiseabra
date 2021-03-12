module Hey.Pages.Home
  ( mkHomePage
  ) where

import Prelude
import Data.Nullable (null)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseScroller (useScrollTrigger)
import Hey.Pages.About (mkAboutPage)
import Hey.Pages.Github (mkGithubPage)
import Hey.Pages.Spacer (mkSpacerPage)
import React.Basic (JSX, fragment)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { threshold :: String
    }

type PageProps
  = { env :: Env
    , route :: Route
    , children :: Array JSX
    }

spacer :: JSX
spacer = DOM.div { style: DOM.css { width: "100vw", height: "100vh" } }

mkScrollTrigger :: Component { id :: String, onEnter :: Effect Unit }
mkScrollTrigger =
  component "ScrollTrigger"
    $ \{ id, onEnter } -> React.do
        ref <- useRef null
        useScrollTrigger ref onEnter
        pure $ DOM.div { ref, id, className: styles.threshold }

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
  scrollTrigger <- mkScrollTrigger
  component "Home"
    $ \env ->
        pure $ fragment $ routes
          <#> \(route /\ c) ->
              let
                onEnter = env.router.replace route
              in
                fragment
                  $ [ scrollTrigger { id: show route, onEnter }
                    , c env
                    ]
