module Hey.Components.Chart
  ( module Hey.Components.Chart.FFI
  , mkChart
  ) where

import Prelude
import Data.Maybe (Maybe(..), maybe)
import Data.Nullable (null)
import Data.Tuple.Nested ((/\))
import Data.Traversable (sequence)
import Hey.Components.Chart.FFI (ChartOptions, ChartData, ChartType(..))
import Hey.Components.Chart.FFI as Chart
import React.Basic.DOM as DOM
import React.Basic.Hooks (Component, component, readRefMaybe, useEffectOnce, useRef, useState)
import React.Basic.Hooks as React
import Web.DOM.Element (fromNode)

mkChart :: Component ChartOptions
mkChart =
  component "Chart"
    $ \options -> React.do
        ref <- useRef null
        chart /\ setChart <- useState Nothing
        useEffectOnce
          $ readRefMaybe ref
          >>= ((=<<) fromNode >>> pure)
          >>= (map (Chart.create options) >>> sequence)
          >>= \c -> do
              setChart (const c)
              pure $ maybe mempty Chart.destroy c
        pure $ DOM.canvas { ref, width: "100%", height: "100%" }
