module Hey.Data.Route
  ( Route(..)
  , match
  , href
  ) where

import Prelude
import Data.Either (either)
import Data.Foldable (oneOf)
import Routing.Hash (match) as Hash
import Routing.Match (Match, lit, end, root)

data Route
  = Home
  | About
  | Projects
  | End
  | NotFound

derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show Home = "home"
  show About = "about"
  show Projects = "projects"
  show End = "fin"
  show NotFound = "404"

routes :: Match Route
routes =
  oneOf
    [ Home <$ (root *> end)
    , About <$ (root *> lit (show About) *> end)
    , Projects <$ (root *> lit (show Projects) *> end)
    , End <$ (root *> lit (show End) *> end)
    , NotFound <$ (root *> lit (show NotFound) *> end)
    , pure NotFound
    ]

match :: String -> Route
match = either (const NotFound) identity <<< Hash.match routes

href :: Route -> String
href Home = "/"

href route = "/" <> show route
