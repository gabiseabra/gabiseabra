module Hey.Components.Repo.Styles where

import Prelude
import Data.Profunctor.Strong ((&&&))
import Data.Tuple.Nested ((/\))
import Web.IntersectionObserverEntry (IntersectionObserverEntry, boundingClientRect, intersectionRatio)

type Styles
  = { card :: String
    , list :: String
    , item :: String
    , positionTop :: String
    , positionBottom :: String
    , positionCenter :: String
    , visibilityFull :: String
    , visibilityPartial :: String
    , visibilityNone :: String
    }

foreign import styles :: Styles

position :: IntersectionObserverEntry -> String
position = (intersectionRatio &&& boundingClientRect) >>> cls
  where
  cls (n /\ { top })
    | n /= 1.0 && top < 0.0 = styles.positionTop
    | n /= 1.0 && top > 0.0 = styles.positionBottom
    | otherwise = styles.positionCenter

visibility :: IntersectionObserverEntry -> String
visibility = intersectionRatio >>> cls
  where
  cls n
    | n == 1.0 = styles.visibilityFull
    | n > 0.0 = styles.visibilityPartial
    | otherwise = styles.visibilityNone
