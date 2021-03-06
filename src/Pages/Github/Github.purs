module Hey.Pages.Github
  ( mkGithubPage
  ) where

import Prelude

import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Hey.Api.Github (fetchViewer)
import Hey.Components.Github.RepoList (mkRepoList)
import Hey.Hooks.UseFetch (useFetch)
import React.Basic.Hooks (Component, component, fragment)

mkGithubPage :: forall a. Component a
mkGithubPage = do
  repoList <- mkRepoList
  component "Repos" \_ -> React.do
    useFetch fetchViewer
      :>>= case _ of
          Nothing -> pure $ mempty
          Just { data: res } -> do
            pure
              $ fragment
                  [ repoList res.viewer.featured.nodes
                  ]
