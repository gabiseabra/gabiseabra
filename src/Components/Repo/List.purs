module Hey.Components.Repo.List (mkRepoList) where

import Prelude
import Data.Array (elem, filter)
import Data.Symbol (class IsSymbol, SProxy(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Hey.Api.Github (Repo)
import Hey.Components.Carousel (Axis(..), mkCarousel, mkSlide)
import Hey.Components.Repo.Card (mkRepoCard)
import Hey.Components.Repo.Stats (mkRepoStats)
import Hey.Components.Repo.Styles (styles)
import Hey.Hooks.Carousel.Controller (carouselProvider, defCarouselOptions, useCarouselController)
import Prim.Row as Row
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, fragment, useMemo)
import React.Basic.Hooks as React
import Record as Rec

isFeatured :: Repo -> Boolean
isFeatured =
  _.name
    >>> (flip elem)
        [ "gabiseabra"
        , "hs-picturefarm"
        , "google-fonts-webpack-plugin"
        ]

mkRepoList :: Component (Array Repo)
mkRepoList = do
  slide <- mkSlide
  card <- mkRepoCard
  stats <- mkRepoStats
  component "Repo"
    $ \repos -> React.do
        let
          opts = defCarouselOptions { threshold = [ 0.0, 1.0 ], rootMargin = "0px" }
        ref /\ value <- useCarouselController opts
        children <-
          useMemo unit \_ ->
            filter isFeatured repos
              # map \repo ->
                  slide
                    { id: "repo-" <> repo.name
                    , className: styles.item
                    , render: Tuple repo >>> card >>> pure
                    }
        pure
          $ fragment
              [ slide
                  { id: "repos-stats"
                  , className: styles.item
                  , render: Tuple repos >>> stats >>> pure
                  }
              , fragment children
              , slide
                  { id: "repos-footer"
                  , className: styles.item
                  , render: const $ pure $ DOM.text "lmao"
                  }
              ]
