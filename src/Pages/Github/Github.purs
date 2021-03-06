module Hey.Pages.Github
  ( mkGithubPage
  ) where

import Prelude

import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Hey.Api.Github (fetchViewer)
import Hey.Components.Github.RepoList (mkRepoList)
import Hey.Data.Env (Env)
import Hey.Hooks.UseFetch (useFetch)
import React.Basic.Hooks (Component, component, fragment)

mkGithubPage :: Component Env
mkGithubPage = do
  repoList <- mkRepoList
  component "Repos" \{ options } -> React.do
    useFetch (fetchViewer options.github.token)
      :>>= case _ of
          Nothing -> pure $ mempty
          Just { data: res } -> do
            pure
              $ fragment
                  [ repoList res.viewer.featured.nodes
                  ]
