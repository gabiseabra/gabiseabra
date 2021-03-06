module Hey.Components.Github.Stats (mkStats) where

import Prelude
import Data.Array (elem, fold, fromFoldable)
import Data.Bifunctor (rmap)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (notNull, null)
import Data.String (toLower)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Hey.Api.Github (User, RepoInfo)
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Chart (ChartOptions)
import Hey.Data.Canvas.Chart as Chart
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Node (appendChild, removeChild)

foreign import styles :: Styles

type Styles
  = { container :: String
    , body :: String
    , stats :: String
    , languages :: String
    }

extension :: String -> String
extension lang
  | lang == "javascript" = "js"
  | lang == "typescript" = "ts"
  | lang == "purescript" = "purs"
  | lang == "haskell" = "hs"
  | lang == "ruby" = "rb"
  | lang == "elixir" = "ex"
  | otherwise = lang

collectData :: Array RepoInfo -> Map String Int
collectData = foldr (_.primaryLanguage >>> _.name >>> Map.alter (maybe 1 ((+) 1) >>> Just)) mempty

uniq :: forall f a. Eq a => Foldable f => Monoid (f a) => Applicative f => f a -> f a
uniq =
  foldl
    ( \as a ->
        if a `elem` as then
          as
        else
          pure a <> as
    )
    mempty

langChartOptions :: Array ({ label :: String, color :: String } /\ Array RepoInfo) -> ChartOptions
langChartOptions =
  map (rmap collectData)
    >>> \x ->
        let
          labels = (map (snd >>> Map.keys) >>> fold >>> fromFoldable >>> uniq) x

          datasets =
            x
              # map \({ label, color } /\ data') ->
                  { color: notNull color
                  , label: notNull label
                  , "data": labels # map ((flip Map.lookup) data' >>> fromMaybe 0)
                  }
        in
          { labels: map (toLower >>> extension) labels, datasets: datasets }

mkLanguagesChart :: Component ChartOptions
mkLanguagesChart = do
  component "Chart"
    $ \options -> React.do
        ref <- useRef null
        chart /\ setChart <- useState Nothing
        useEffectOnce
          $ readRefMaybe ref
          >>= maybe (pure mempty) \node -> do
              c <- Chart.mkCanvas options
              void $ appendChild (Canvas.toNode c) node
              setChart (const $ Just c)
              pure $ void $ removeChild (Canvas.toNode c) node
        pure $ DOM.div { className: styles.languages, ref }

mkStats :: Component User
mkStats = do
  langChart <- mkLanguagesChart
  component "Repo"
    $ \user -> React.do
        let
          data' =
            [ { label: "my repos", color: pink } /\ user.repositories.nodes
            , { label: "contributions", color: purple } /\ user.contributions.nodes
            ]
        pure
          $ DOM.div
              { className: styles.container
              , children:
                  [ DOM.div
                      { className: styles.body
                      , children:
                          [ langChart $ langChartOptions data'
                          ]
                      }
                  ]
              }
  where
  pink = "rgb(247, 99, 153)"

  purple = "rgb(141, 149, 236)"
