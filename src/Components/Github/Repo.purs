module Hey.Components.Github.Repo (mkRepo) where

import Prelude
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as FDT
import Data.List (fromFoldable)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (null)
import Hey.API.Github (Repo)
import Hey.Components.SVG.Icon (Icon(..), icon)
import Hey.Components.Typography (FontSize(..), Heading(..))
import Hey.Components.Typography as Typo
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
              , children =
                [ DOM.span_ $ pure $ DOM.text "Published on"
                , DOM.span_ $ pure $ DOM.text $ FDT.format format (unwrap createdAt)
                ]
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
        [ nonEmpty homepageUrl
            # maybe mempty \href ->
                DOM.a { href, children: [ icon ExternalLink ] }
        , DOM.a
            { href: url
            , children: [ icon Github ]
            }
        ]
    }
  where
  nonEmpty (Just "") = Nothing

  nonEmpty x = x

placeholder :: String
placeholder = "lmaoo"

mkRepo :: Component Repo
mkRepo = do
  component "GithubRepo"
    $ \repo@{ name, description } -> React.do
        pure
          $ DOM.article
              { className: styles.container
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
