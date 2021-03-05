module Hey.Data.Canvas
  ( kind Scene
  , Canvas
  , toNode
  , destroy
  ) where

import Prelude
import Effect (Effect)
import Web.DOM (Node)

foreign import kind Scene

-- TODO optimization: remove scene param and render all in a single canvas
foreign import data Canvas :: Scene -> Type

foreign import toNode :: forall a. Canvas a -> Node

foreign import destroy :: forall a. Canvas a -> Effect Unit
