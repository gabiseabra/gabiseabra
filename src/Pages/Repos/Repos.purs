module Hey.Pages.Repos
  ( mkReposPage
  ) where

import Prelude
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Hey.Api.Github (fetchRepos)
import Hey.Components.Repo.List (mkRepoList)
import Hey.Hooks.UseFetch (useFetch)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { page :: String
    , content :: String
    }

mkReposPage :: forall a. Component a
mkReposPage = do
  repoList <- mkRepoList
  component "Repos" \_ -> React.do
    useFetch fetchRepos
      :>>= case _ of
          Nothing -> pure $ DOM.text "loading..."
          Just { data: res } -> do
            pure $ repoList res.viewer.repositories.nodes
