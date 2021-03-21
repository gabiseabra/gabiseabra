module Hey.Components.Github.Stats (mkStats) where

import Prelude
import Data.Array as Array
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (null)
import Data.Number.Format as Num
import Data.String as String
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Hey.API.Github (User, RepoInfo)
import Hey.Components.SVG.Icon (Icon(..), icon)
import Hey.Components.Typography as Typo
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Chart (ChartOptions)
import Hey.Data.Canvas.Chart as Chart
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, readRefMaybe, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Node as Node

foreign import styles :: Styles

type Styles
  = { container :: String
    , body :: String
    , stats :: String
    , languages :: String
    , info :: String
    , infoHead :: String
    , infoScale :: String
    , infoScaleBar :: String
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
collectData = Array.foldr (_.primaryLanguage >>> _.name >>> Map.alter (maybe 1 ((+) 1) >>> Just)) mempty

uniq :: forall a. Eq a => Monoid (Array a) => Array a -> Array a
uniq =
  Array.foldl
    ( \as a ->
        if a `Array.elem` as then
          as
        else
          pure a <> as
    )
    mempty

langChartOptions :: Array (Array RepoInfo) -> ChartOptions
langChartOptions =
  map collectData
    >>> \x ->
        let
          langs = x # map Map.keys >>> Array.fold >>> Array.fromFoldable >>> uniq

          labels = langs # map (String.toLower >>> extension)

          mkDataset d = langs # map ((flip Map.lookup) d >>> fromMaybe 0)

          data0 = Array.replicate (Array.length langs) 0

          data' = x # map mkDataset >>> Array.foldr (Array.zipWith (+)) data0
        in
          { labels, datasets: [ { label: null, "data": data' } ] }

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
              void $ Node.appendChild (Canvas.toNode c) node
              setChart (const $ Just c)
              pure
                $ do
                    Canvas.destroy c
                    void $ Node.removeChild (Canvas.toNode c) node
        pure $ DOM.div { className: styles.languages, ref }

infoHead :: Icon -> String -> Int -> JSX
infoHead ic label value =
  DOM.header
    { className: styles.infoHead
    , children:
        [ DOM.div_
          [ icon ic
          , Typo.mark [ Typo.span_ [ DOM.text (label <> ":") ] ]
          ]
        , Typo.span
            $ Typo.spanProps
                { bold = true
                , children = [ DOM.text $ show value ]
                }
        ]
    }

type ScaleItem
  = String /\ Int

infoScale :: Array ScaleItem -> JSX
infoScale l =
  DOM.div
    { className: styles.infoScale
    , children:
        l
          <#> \(label /\ n) ->
              DOM.div
                { style: DOM.css { width: pct 6 n }
                , children:
                    pure
                      $ DOM.div_
                          [ Typo.span_ $ pure $ DOM.text $ label <> ":"
                          , Typo.span_ $ pure $ DOM.text $ pct 2 n
                          ]
                }
    }
  where
  max = Array.foldr (Tuple.snd >>> (+)) 0 l

  ratio n = Int.toNumber n / Int.toNumber max

  pct p n = (Num.toStringWith (Num.fixed p) $ ratio n * 100.0) <> "%"

userInfo :: User -> JSX
userInfo user =
  DOM.section
    { className: styles.info
    , children:
        [ DOM.article_
            $ [ infoHead Code "repos" (forks + repos)
              , infoScale [ "forks" /\ forks, "sources" /\ repos ]
              ]
        , DOM.article_ $ pure $ infoHead HandsHelping "contributed to" contrib
        , DOM.article_ $ pure $ infoHead CodeBranch "PRs" prs
        , DOM.article_ $ pure $ infoHead Nag "issues" issues
        , DOM.article_ $ pure $ infoHead Star "stars" stars
        ]
    }
  where
  forks = user.forks.totalCount

  repos = user.repositories.totalCount

  contrib = user.contributions.totalCount

  stars = Array.foldr (_.stargazerCount >>> (+)) 0 user.repositories.nodes

  issues = user.issues.totalCount

  prs = user.pullRequests.totalCount

mkStats :: Component User
mkStats = do
  langChart <- mkLanguagesChart
  component "Repo"
    $ \user -> React.do
        let
          data' =
            [ user.repositories.nodes
            , user.contributions.nodes
            ]
        pure
          $ DOM.div
              { className: styles.container
              , children:
                  [ DOM.div
                      { className: styles.body
                      , children:
                          [ langChart $ langChartOptions data'
                          , userInfo user
                          ]
                      }
                  ]
              }
