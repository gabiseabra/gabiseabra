module Hey.Components.Repo.Stats (mkRepoStats) where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (foldl, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.String (toLower)
import Data.Tuple.Nested (type (/\), (/\))
import Debug.Trace as Debug
import Hey.Api.Github (Repo)
import Hey.Components.Chart (ChartOptions, ChartType(..), mkChart)
import Hey.Components.Repo.Styles (styles)
import Hey.Components.Repo.Styles as Cls
import Hey.Extra.Styles ((.&))
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component)
import Web.IntersectionObserverEntry (IntersectionObserverEntry)

type StatsProps
  = Array Repo /\ Maybe IntersectionObserverEntry

extension :: String -> String
extension lang
  | lang == "javascript" = ".js"
  | lang == "typescript" = ".ts"
  | lang == "purescript" = ".ps"
  | lang == "haskell" = ".hs"
  | lang == "ruby" = ".rb"
  | lang == "elixir" = ".ex"
  | otherwise = "." <> lang

languagesChart :: Array Repo -> ChartOptions
languagesChart repos =
  Debug.spy "lang"
    { type: Radar
    , "data":
        { labels
        , datasets: [ { label: null, "data": values } ]
        }
    }
  where
  dataset = foldr (_.primaryLanguage >>> _.name >>> Map.alter (maybe 1 ((+) 1) >>> Just)) mempty repos

  labels = map (toLower >>> extension) $ fromFoldable $ Map.keys dataset

  values = fromFoldable $ Map.values dataset

mkRepoStats :: Component StatsProps
mkRepoStats = do
  chart <- mkChart
  component "Repo"
    $ \(repos /\ entry) -> React.do
        pure
          $ DOM.div
              { className:
                  foldl (.&)
                    mempty
                    [ styles.stats
                    , maybe mempty Cls.visibility entry
                    ]
              , children:
                  [ DOM.div
                      { className: styles.languagesChart
                      , children: [ chart { width: "100%", height: "100%", options: languagesChart repos } ]
                      }
                  ]
              }
