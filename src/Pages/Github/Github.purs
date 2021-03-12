module Hey.Pages.Github
  ( mkGithubPage
  ) where

import Prelude
import Control.Monad.Indexed ((:>>=))
import Data.Maybe (Maybe(..))
import Data.Nullable (null)
import Hey.API.Github (Repo, fetchViewer)
import Hey.Components.Github.Repo (mkRepo)
import Hey.Data.Env (Env)
import Hey.Data.Route (Route(..))
import Hey.Hooks.UseFetch (useFetch)
import React.Basic.Hooks (Component, component, fragment, useRef)

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
