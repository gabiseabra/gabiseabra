module Hey.Components.Repo.List (mkRepoList) where

import Prelude
import Data.Array (elem, filter)
import Data.Tuple.Nested ((/\))
import Hey.Api.Github (Repo)
import Hey.Components.Carousel (Axis(..), mkCarousel, mkSlide)
import Hey.Components.Repo.Card (mkRepoCard)
import Hey.Components.Repo.Styles (styles)
import Hey.Hooks.Carousel.Controller (carouselProvider, defCarouselOptions, useCarouselController)
import React.Basic.Hooks (Component, component, useMemo)
import React.Basic.Hooks as React

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
  carousel <- mkCarousel
  slide <- mkSlide
  card <- mkRepoCard
  component "Repo"
    $ \repos -> React.do
        let
          opts = defCarouselOptions { threshold = [ 0.0, 1.0 ], rootMargin = "25% 0px" }
        ref /\ value <- useCarouselController opts
        children <-
          useMemo unit \_ ->
            filter isFeatured repos
              # map \repo ->
                  slide
                    { id: "repo-" <> repo.name
                    , className: styles.item
                    , render: \intersectionEntry -> pure $ card { data: repo, intersectionEntry }
                    }
        pure
          $ carouselProvider
              { value
              , children:
                  pure
                    $ carousel
                        { ref
                        , axis: Y
                        , className: styles.list
                        , children
                        }
              }
