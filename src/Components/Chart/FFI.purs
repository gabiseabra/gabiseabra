module Hey.Components.Chart.FFI
  ( Chart
  , ChartType(..)
  , DataSet
  , ChartData
  , ChartOptions
  , create
  , destroy
  , reset
  , update
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable)
import Data.String (toLower)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Hey.Data.Easing (Easing)
import Web.DOM (Element)

data ChartType
  = Radar

derive instance genericChartType :: Generic ChartType _

instance showChartType :: Show ChartType where
  show = genericShow >>> toLower

data Chart

type DataSet =
  { label :: Nullable String
  , data :: Array Int
  }

type ChartData
  = { labels :: Array String
    , datasets :: Array DataSet
    }

type ChartOptions_
  = { type :: String
    , data :: ChartData
    }

type ChartOptions
  = { type :: ChartType
    , data :: ChartData
    }

foreign import create_ :: ChartOptions_ -> Element -> Effect Chart

create :: ChartOptions -> Element -> Effect Chart
create opts = create_ (opts { "type" = show opts.type })

foreign import destroy :: Chart -> Effect Unit

foreign import reset :: Chart -> Effect Unit

foreign import update_ :: Number -> String -> Chart -> Effect Unit

update :: Milliseconds -> Easing -> Chart -> Effect Unit
update ms e = update_ (unwrap ms) (show e)
