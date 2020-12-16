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
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Hey.Api.Github (Repo)
import Hey.Components.Github.Repo (mkRepo)
import Hey.Hooks.UseSnapPoints (useSnapPoint)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, readRefMaybe, useEffectOnce, useRef)
import React.Basic.Hooks as React
import Web.DOM (Node)
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

foreign import styles :: Styles

foreign import animate :: EffectFn1 Node (Effect Unit)

type Styles
  = { container :: String
    , scroller :: String
    , scene :: String
    , list :: String
    , item :: String
    }

carousel :: Array JSX -> JSX
carousel children =
  DOM.div
    { className: styles.list
    , children:
        children
          # map \child ->
              DOM.div
                { className: styles.item
                , children: [ child ]
                }
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
        scrollerRef <- useRef null
        useEffectOnce
          $ readRefMaybe scrollerRef
          >>= maybe (throw "No container ref") (runEffectFn1 animate)
        useSnapPoint "github/repos"
          $ readRefMaybe scrollerRef
          >>= ((=<<) HTMLElement.fromNode)
          >>> maybe (throw "No container ref") (snapPoints $ length repos)
        pure
          $ DOM.div
              { className: styles.container
              , children:
                  [ DOM.div
                      { ref: scrollerRef
                      , className: styles.scroller
                      , children:
                          pure
                            $ DOM.div
                                { className: styles.scene
                                , children: pure $ carousel $ map repo repos
                                }
                      }
                  ]
              }
