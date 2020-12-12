module Hey.Styles where

import Prelude
import Data.Profunctor.Strong ((&&&))
import Data.Tuple.Nested ((/\))
import Web.IntersectionObserverEntry (IntersectionObserverEntry, boundingClientRect, intersectionRatio)

condClass :: Boolean -> String -> String
condClass true cls = cls

condClass false cls = ""

infixr 6 condClass as ?&

appendClass :: String -> String -> String
appendClass l "" = l

appendClass "" r = r

appendClass l r = l <> " " <> r

infixr 6 appendClass as .&

type PositionStyles r
  = ( positionTop :: String
    , positionBottom :: String
    , positionCenter :: String
    | r
    )

position :: forall r. { | PositionStyles r } -> IntersectionObserverEntry -> String
position styles = (intersectionRatio &&& boundingClientRect) >>> cls
  where
  cls (n /\ { top })
    | n /= 1.0 && top < 0.0 = styles.positionTop
    | n /= 1.0 && top > 0.0 = styles.positionBottom
    | otherwise = styles.positionCenter

type VisibilityStyles r
  = ( visibilityFull :: String
    , visibilityPartial :: String
    , visibilityNone :: String
    | r
    )

visibility :: forall r. { | VisibilityStyles r } -> IntersectionObserverEntry -> String
visibility styles = intersectionRatio >>> cls
  where
  cls n
    | n == 1.0 = styles.visibilityFull
    | n > 0.0 = styles.visibilityPartial
    | otherwise = styles.visibilityNone
