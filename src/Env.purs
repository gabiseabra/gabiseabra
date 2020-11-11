module Hey.Env where

import Prelude

import Data.Bifunctor (rmap)
import Data.Tuple.Nested (type (/\))
import Effect (Effect)
import Hey.Router (Router, mkRouter)
import React.Basic (JSX)

type Env =
  { router :: Router
  }

mkEnv' router = { router }

mkEnv :: Effect (JSX /\ Env)
mkEnv = mkRouter >>= rmap mkEnv' >>> pure
