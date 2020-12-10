module Hey.Data.Easing
  ( EasingType(..)
  , EasingStage(..)
  , Easing(..)
  ) where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

data EasingType
  = Bounce
  | Cubic
  | Quad
  | Quint
  | Sine
  | Expo
  | Circ
  | Back

derive instance genericEasingType :: Generic EasingType _

instance showEasingType :: Show EasingType where
  show = genericShow

data EasingStage
  = In
  | Out
  | InOut

derive instance genericEasingStage :: Generic EasingStage _

instance showEasingStage :: Show EasingStage where
  show = genericShow

data Easing
  = Linear
  | Ease EasingStage EasingType

instance showEasing :: Show Easing where
  show Linear = "linear"
  show (Ease s t) = "ease" <> show s <> show t
