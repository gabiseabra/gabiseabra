module Hey.Pages.Github
  ( mkGithubPage
  ) where

import Prelude
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Hey.API.Github (fetchViewer)
import Hey.Components.Github.Repo (mkRepo)
import Hey.Data.Env (Env)
import Hey.Hooks.UseFetch (useFetch)
import React.Basic.Hooks (Component, component, fragment)

mkGithubPage :: Component Env
mkGithubPage = do
  repo <- mkRepo
  component "Repos" \{ options } -> React.do
    useFetch (fetchViewer options.github.token)
      :>>= case _ of
          Nothing -> pure $ mempty
          Just { data: res } -> do
            let
              repos = res.viewer.featured.nodes
            pure $ fragment $ repos <#> \r -> repo r
