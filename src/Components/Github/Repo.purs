module Hey.Components.Github.Repo (mkRepo) where

import Prelude
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.Formatter.DateTime as FDT
import Data.List (fromFoldable)
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Nullable (null)
import Hey.Api.Github (Repo)
import Hey.Components.SVG.Icon (Icon(..), icon)
import Hey.Components.Typography (FontSize(..), Heading(..))
import Hey.Components.Typography as Typo
import Hey.Hooks.UseScroll (usePerspective)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, useRef)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { container :: String
    , repo :: String
    , languages :: String
    , date :: String
    , links :: String
    }

lipsum = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam sodales eros lectus, eu lacinia ligula imperdiet vel. Nunc varius mattis enim non lacinia. Nulla facilisi. Donec id facilisis nisl. Integer faucibus nisl ut quam finibus blandit. Cras diam mauris, ornare sit amet velit nec, egestas molestie justo. Fusce quis blandit tellus."

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
        [ DOM.a
            { href: url
            , children: [ icon Github ]
            }
        , homepageUrl
            # maybe mempty \href ->
                DOM.a { href, children: [ icon ExternalLink ] }
        ]
    }

mkRepo :: Component Repo
mkRepo =
  component "GithubRepo"
    $ \repo@{ name, description } -> React.do
        ref <- useRef null
        style <- usePerspective ref
        pure
          $ DOM.div
              { ref
              , style
              , className: styles.container
              , children:
                  pure
                    $ DOM.article
                        { className: styles.repo
                        , children:
                            [ Typo.h H2 $ pure $ DOM.text name
                            , Typo.p_ $ pure $ DOM.text $ fromMaybe lipsum description
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
