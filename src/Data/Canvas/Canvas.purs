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

foreign import data Canvas :: Scene -> Type

foreign import toNode :: forall a. Canvas a -> Node

foreign import destroy :: forall a. Canvas a -> Effect Unit
