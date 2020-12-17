module Hey.Components.Github.Repo (mkRepo) where

import Prelude
import Data.Maybe (maybe)
import Hey.Api.Github (Repo)
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import React.Basic.Hooks as React

foreign import styles :: Styles

type Styles
  = { container :: String
    , repo :: String
    }

mkRepo :: Component Repo
mkRepo =
  component "GithubRepo"
    $ \{ name, description, languages } -> React.do
        pure
          $ DOM.div
              { className: styles.container
              , children:
                  pure
                    $ DOM.article
                        { className: styles.repo
                        , children:
                            [ DOM.h2_ [ DOM.text name ]
                            , DOM.p_ [ maybe mempty DOM.text description ]
                            , DOM.footer_
                                $ languages.nodes
                                <#> \lang -> DOM.span_ [ DOM.text lang.name ]
                            ]
                        }
              }
