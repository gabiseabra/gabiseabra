module Hey.Data.Canvas
  ( kind CanvasElement
  , Canvas
  , toNode
  ) where

import Web.DOM (Node)

foreign import kind CanvasElement

foreign import data Canvas :: CanvasElement -> Type

foreign import toNode :: forall a. Canvas a -> Node
