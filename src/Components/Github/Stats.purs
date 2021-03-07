module Hey.Components.Github.Stats (mkStats) where

import Prelude
import Data.Array (elem, fold, fromFoldable)
import Data.Bifunctor (rmap)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Int as Int
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Nullable (notNull, null)
import Data.String (toLower)
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Hey.Api.Github (User, RepoInfo)
import Hey.Components.SVG.Icon (Icon(..), icon)
import Hey.Components.Typography as Typo
import Hey.Data.Canvas as Canvas
import Hey.Data.Canvas.Chart (ChartOptions)
import Hey.Data.Canvas.Chart as Chart
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, JSX, component, readRefMaybe, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Node (appendChild, removeChild)

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

langChartOptions :: Array (String /\ Array RepoInfo) -> ChartOptions
langChartOptions =
  map (rmap collectData)
    >>> \x ->
        let
          labels = (map (snd >>> Map.keys) >>> fold >>> fromFoldable >>> uniq) x

          datasets =
            x
              # map \(label /\ data') ->
                  { label: notNull label
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

infoHead :: Icon -> String -> Int -> JSX
infoHead ic label value =
  DOM.header
    { className: styles.infoHead
    , children:
        [ icon ic
        , Typo.mark [ Typo.span_ [ DOM.text (label <> ":") ] ]
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
          <#> \(label /\ x) ->
              DOM.div
                { style: DOM.css { width: width x }
                , children: pure $ Typo.span_ [ DOM.text label ]
                }
    }
  where
  max = foldr (snd >>> (+)) 0 l

  ratio n = Int.toNumber n / Int.toNumber max

  width n = (show $ Int.ceil $ (ratio n * 100.0)) <> "%"

userInfo :: User -> JSX
userInfo user =
  DOM.section
    { className: styles.info
    , children:
        pure
          $ DOM.div_
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

  stars = foldr (_.stargazerCount >>> (+)) 0 user.repositories.nodes

  issues = user.issues.totalCount

  prs = user.pullRequests.totalCount

mkStats :: Component User
mkStats = do
  langChart <- mkLanguagesChart
  component "Repo"
    $ \user -> React.do
        let
          data' =
            [ "my repos" /\ user.repositories.nodes
            , "contributions" /\ user.contributions.nodes
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
