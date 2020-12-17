module Hey.Components.Github.Repo (mkRepo) where

import Prelude
import Data.Maybe (fromMaybe)
import Hey.Api.Github (Repo)
import Hey.Components.Typography (FontSize(..), Heading(..))
import Hey.Components.Typography as Typo
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component)

foreign import styles :: Styles

type Styles
  = { container :: String
    , repo :: String
    , languages :: String
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

mkRepo :: Component Repo
mkRepo =
  component "GithubRepo"
    $ \repo@{ name, description } -> React.do
        pure
          $ DOM.div
              { className: styles.container
              , children:
                  pure
                    $ DOM.article
                        { className: styles.repo
                        , children:
                            [ Typo.h H2 $ pure $ DOM.text name
                            , Typo.p_ $ pure $ DOM.text $ fromMaybe lipsum description
                            , DOM.footer
                                { children:
                                    [ languages repo ]
                                }
                            ]
                        }
              }
