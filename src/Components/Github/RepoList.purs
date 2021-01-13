module Hey.Components.Github.RepoList (mkRepoList) where

import Prelude
import Data.Array (length)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Data.Unfoldable (unfoldr)
import Effect (Effect)
import Effect.Exception (throw)
import Hey.Api.Github (Repo)
import Hey.Components.Github.Repo (mkRepo)
import Hey.Hooks.UseScroll (useSnapPoint)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useRef)
import React.Basic.Hooks as React
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

foreign import styles :: Styles

type Styles
  = { container :: String
    , item :: String
    }

-- | Calculate snap points centered at each slide, relative to the container element
snapPoints :: Int -> HTMLElement -> Effect (Array Number)
snapPoints count el = do
  vh <- window >>= Window.innerHeight >>= toNumber >>> pure
  y <- HTMLElement.offsetTop el
  h <- HTMLElement.offsetHeight el
  let
    x0 = y - vh * 0.25

    xh = h `div` toNumber (count - 1)

    foldx n
      | n == -1 = Nothing
      | otherwise = Just ((x0 + (xh * toNumber n)) /\ (n - 1))
  pure $ unfoldr foldx (count - 1)

mkRepoList :: Component (Array Repo)
mkRepoList = do
  repo <- mkRepo
  component "GithubRepoList"
    $ \repos -> React.do
        containerRef <- useRef null
        useSnapPoint "github/repos"
          $ readRefMaybe containerRef
          >>= ((=<<) HTMLElement.fromNode)
          >>> maybe (throw "No container ref") (snapPoints $ length repos)
        pure
          $ DOM.div
              { ref: containerRef
              , className: styles.container
              , children:
                  repos
                    <#> \r ->
                        DOM.div
                          { className: styles.item
                          , children: [ repo r ]
                          }
              }
