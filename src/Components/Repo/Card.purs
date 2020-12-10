module Hey.Components.Repo.Card (mkRepoCard) where

import Prelude
import Data.Foldable (foldl)
import Data.Maybe (Maybe, maybe)
import Data.Tuple.Nested (type (/\), (/\))
import Hey.Api.Github (Repo)
import Hey.Components.Repo.Styles (styles)
import Hey.Components.Repo.Styles as Cls
import Hey.Extra.Styles ((.&))
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import Web.IntersectionObserverEntry (IntersectionObserverEntry)

type CardProps
  = Repo /\ Maybe IntersectionObserverEntry

mkRepoCard :: Component CardProps
mkRepoCard =
  component "Repo"
    $ \(repo /\ entry) -> React.do
        pure
          $ DOM.div
              { className:
                  foldl (.&)
                    mempty
                    [ styles.card
                    , maybe mempty Cls.position entry
                    , maybe mempty Cls.visibility entry
                    ]
              , children:
                  [ DOM.text repo.name
                  ]
              }
