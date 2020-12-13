module Hey.Components.Github.RepoList (mkRepoList) where

import Prelude
import Data.Array (take)
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Effect.Exception (throw)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Hey.Api.Github (Repo)
import Hey.Components.Github.Repo (mkRepo)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, readRefMaybe, useEffectOnce, useRef)
import React.Basic.Hooks as React
import Web.DOM (Node)

foreign import styles :: Styles

foreign import animate :: EffectFn1 Node Unit

type Styles
  = { list :: String
    , item :: String
    , scene :: String
    , container :: String
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

mkRepoList :: Component (Array Repo)
mkRepoList = do
  repo <- mkRepo
  component "GithubRepoList"
    $ \repos -> React.do
        let
          repos' = take 6 repos
        containerRef <- useRef null
        sceneRef <- useRef null
        useEffectOnce
          $ readRefMaybe containerRef
          >>= case _ of
              Nothing -> throw "No container ref"
              Just node -> do
                runEffectFn1 animate node
                pure mempty
        pure
          $ DOM.div
              { ref: containerRef
              , className: styles.container
              , children:
                  pure
                    $ DOM.div
                        { className:
                            styles.scene
                        , children: pure $ carousel $ map repo repos'
                        }
              }
