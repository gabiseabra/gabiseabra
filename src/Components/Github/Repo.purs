module Hey.Components.Github.Repo (mkRepo, mkRepoList) where

import Prelude
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as FDT
import Data.List (fromFoldable)
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (null)
import Hey.API.Github (Repo)
import Hey.Components.SVG.Icon (Icon(..), icon)
import Hey.Components.Typography (FontSize(..), Heading(..))
import Hey.Components.Typography as Typo
import Hey.Hooks.UseScroll (useSnapPoint)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, fragment, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { container :: String
    , repo :: String
    , languages :: String
    , date :: String
    , links :: String
    }

languages :: Repo -> JSX
languages repo =
  DOM.div
    { className: styles.languages
    , children:
        [ Typo.mark
            $ pure
            $ Typo.span
                Typo.spanProps
                  { bold = true
                  , children = [ DOM.text "Languages:" ]
                  }
        , DOM.ul_
            $ repo.languages.nodes
            <#> \lang ->
                DOM.li_
                  [ Typo.span
                      Typo.spanProps
                        { fontSize = Small
                        , children = [ DOM.text lang.name ]
                        }
                  ]
        ]
    }

pubDate :: Repo -> JSX
pubDate { createdAt } =
  DOM.div
    { className: styles.date
    , children:
        [ icon Clock
        , Typo.span
            Typo.spanProps
              { fontSize = Small
              , children = pure $ DOM.text $ "Published on " <> FDT.format format (unwrap createdAt)
              }
        ]
    }
  where
  format = fromFoldable [ MonthShort, Placeholder " ", YearFull ]

links :: Repo -> JSX
links { url, homepageUrl } =
  DOM.div
    { className: styles.links
    , children:
        [ homepageUrl
            # maybe mempty \href ->
                DOM.a { href, children: [ icon ExternalLink ] }
        , DOM.a
            { href: url
            , children: [ icon Github ]
            }
        ]
    }

placeholder :: String
placeholder = "lmaoo"

mkRepo :: Component Repo
mkRepo = do
  component "GithubRepo"
    $ \repo@{ name, description } -> React.do
        ref <- useRef null
        useSnapPoint ref
        pure
          $ DOM.article
              { ref
              , className: styles.container
              , children:
                  pure
                    $ DOM.div
                        { className: styles.repo
                        , children:
                            pure
                              $ DOM.div_
                                  [ Typo.h H2 $ pure $ DOM.text name
                                  , Typo.p_ $ pure $ Typo.autoLink $ fromMaybe placeholder description
                                  , DOM.footer
                                      { children:
                                          [ languages repo
                                          , pubDate repo
                                          , links repo
                                          ]
                                      }
                                  ]
                        }
              }

mkRepoList :: Component (Array Repo)
mkRepoList = do
  repo <- mkRepo
  component "GithubRepoList"
    $ \repos -> React.do
        pure $ fragment $ repos <#> \r -> repo r
