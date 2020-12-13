module Hey.Components.Github.Stats (mkStats) where

import Prelude
import Data.Array (fromFoldable)
import Data.Foldable (foldl, foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (notNull, null)
import Data.String (toLower)
import Hey.Api.Github (Repo)
import Hey.Components.Chart (ChartOptions, ChartType(..), mkChart)
import Hey.Hooks.UseIntersectionObserver (useIntersectionObserverEntry)
import Hey.Styles (VisibilityStyles, (.&))
import Hey.Styles as Cls
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, useRef)
import React.Basic.Hooks as React
import Type.Row (type (+))

foreign import styles :: Styles

type Styles
  = { container :: String
    , stats :: String
    , languages :: String
    }

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
  { type: Radar
  , "data":
      { labels
      , datasets: [ { label: notNull "my repos", "data": values } ]
      }
  }
  where
  dataset = foldr (_.primaryLanguage >>> _.name >>> Map.alter (maybe 1 ((+) 1) >>> Just)) mempty repos

  labels = map (toLower >>> extension) $ fromFoldable $ Map.keys dataset

  values = fromFoldable $ Map.values dataset

mkStats :: Component (Array Repo)
mkStats = do
  chart <- mkChart
  component "Repo"
    $ \repos -> React.do
        ref <- useRef null
        entry <- useIntersectionObserverEntry ref
        pure
          $ DOM.div
              { ref
              , className: styles.container
              , children:
                  pure
                    $ DOM.div
                        { className: styles.stats
                        , children:
                            [ DOM.div
                                { className: styles.languages
                                , children: [ chart { width: "500px", height: "500px", options: languagesChart repos } ]
                                }
                            ]
                        }
              }
