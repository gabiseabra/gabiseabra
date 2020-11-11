module Hey.Router where

import Prelude

import Data.Either (either)
import Data.Foldable (oneOf)
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Foreign (unsafeToForeign)
import React.Basic.Hooks (JSX, component, useEffectOnce)
import React.Basic.Hooks as React
import Routing.Hash as Hash
import Routing.Match (Match, lit, end, root)
import Web.HTML (window)
import Web.HTML.History as History
import Web.HTML.Window as Window
import Wire.Signal (Signal)
import Wire.Signal as Signal

data Route
  = Home
  | About
  | NotFound

derive instance eqRoute :: Eq Route
instance showRoute :: Show Route where 
  show Home = ""
  show About = "about"
  show NotFound = "404"

type Router =
    { signal :: Signal Route
    , push :: Route -> Effect Unit
    , replace :: Route -> Effect Unit
    }

routes :: Match Route
routes = oneOf
  [ Home <$ (root *> end)
  , About <$ (root *> lit (show About) *> end)
  , NotFound <$ (root *> lit (show NotFound) *> end)
  , pure NotFound
  ]

match :: String -> Route
match hash = either (const NotFound) identity $ Hash.match routes ("/" <> hash)

href :: Route -> String
href route = "#" <> show route

mkRouter :: Effect (JSX /\ Router)
mkRouter = do
  let stateFn fun r =
        window
          >>= Window.history
          >>= fun (unsafeToForeign {})
                  (History.DocumentTitle "")
                  (History.URL $ href r)
      push = stateFn History.pushState
      replace = stateFn History.replaceState
  { signal , modify } <- Hash.getHash >>= match >>> Signal.create
  listener <- component "RouteListener" $ \_ -> React.do
                useEffectOnce $ Hash.hashes (const $ match >>> const >>> modify)
                mempty
  pure $ listener {} /\ { signal, push, replace }
