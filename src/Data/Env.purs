module Hey.Data.Env where

import Prelude

import Effect (Effect)
import Hey.Data.Route (Route)
import Wire.Signal (Signal)

type Router =
  { signal :: Signal Route
  , push :: Route -> Effect Unit
  , replace :: Route -> Effect Unit
  }

type Env =
  { router :: Router
  }
