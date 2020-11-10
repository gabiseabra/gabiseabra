module Hey.Styles where

import Prelude

condClass :: Boolean -> String -> String
condClass true cls = " " <> cls
condClass false _ = ""

infixr 6 condClass as ?&
