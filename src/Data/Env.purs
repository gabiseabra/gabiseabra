module Hey.Data.Env where

import Prelude
import Effect (Effect)
import Hey.Data.Canvas (Canvas)
import Hey.Data.Canvas.Background (Background)
import Hey.Data.Route (Route)
import Wire.Signal (Signal)

type Options
  = { github :: { token :: String }
    }

-- TODO could use argonaut or something for validation
foreign import getOptions :: Effect Options

type Router
  = { signal :: Signal Route
    , push :: Route -> Effect Unit
    , replace :: Route -> Effect Unit
    }

type Env
  = { router :: Router
    , options :: Options
    }
