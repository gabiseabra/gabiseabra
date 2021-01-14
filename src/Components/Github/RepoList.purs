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
import Hey.Hooks.UseScroll (useSnapPoint, offsetTop)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, fragment, readRefMaybe, useRef)
import React.Basic.Hooks as React
import Web.HTML (HTMLElement, window)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

mkRepoList :: Component (Array Repo)
mkRepoList = do
  repo <- mkRepo
  component "GithubRepoList"
    $ \repos -> React.do
        pure $ fragment $ repos <#> \r -> repo r
