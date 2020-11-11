module Hey.Styles where

import Prelude

condClass :: Boolean -> String -> String
condClass true cls = cls
condClass false cls = ""

infixr 6 condClass as ?&

appendClass :: String -> String -> String
appendClass l "" = l
appendClass "" r = r
appendClass l r = l <> " " <> r

infixr 6 appendClass as .&
