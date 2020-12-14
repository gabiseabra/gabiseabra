module Hey.Components.Chart
  ( module Hey.Components.Chart.FFI
  , RequiredChartProps
  , OptionalChartProps
  , ChartProps
  , mkChart
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Traversable (sequence)
import Data.Tuple.Nested ((/\))
import Debug.Trace as Debug
import Hey.Components.Chart.FFI (ChartOptions, ChartData, ChartType(..))
import Hey.Components.Chart.FFI as Chart
import Hey.Extra.Props (PropsRep(..), runProps)
import Prim.Row as Row
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Element (fromNode)

type RequiredChartProps r
  = ( options :: ChartOptions | r )

type OptionalChartProps
  = ( width :: String
    , height :: String
    , className :: String
    )

type ChartProps r
  = Record (RequiredChartProps r)

chartProps =
  DefProps
    { width: "100%"
    , height: "100%"
    , className: ""
    } ::
    PropsRep (RequiredChartProps ()) OptionalChartProps

mkChart :: forall opts opts'. Row.Union opts opts' OptionalChartProps => Component (ChartProps opts)
mkChart =
  component "Chart"
    $ runProps chartProps
    >>> \{ width, height, options } -> React.do
        ref <- useRef null
        chart /\ setChart <- useState Nothing
        useEffectOnce
          $ readRefMaybe ref
          >>= ((=<<) fromNode >>> pure)
          >>= (map (Chart.create options) >>> sequence)
          >>= \c -> do
              setChart (const c)
              pure $ maybe mempty Chart.destroy c
        pure $ DOM.canvas { ref, width, height }
