module Hey.Data.Canvas.Chart
  ( DataSet
  , ChartOptions
  , Chart
  , mkCanvas
  ) where

import Data.Nullable (Nullable)
import Effect (Effect)
import Hey.Data.Canvas (kind Scene, Canvas)

type DataSet
  = { label :: Nullable String
    , color :: Nullable String
    , data :: Array Int
    }

type ChartOptions
  = { labels :: Array String
    , datasets :: Array DataSet
    }

foreign import data Chart :: Scene

foreign import mkCanvas :: ChartOptions -> Effect (Canvas Chart)
