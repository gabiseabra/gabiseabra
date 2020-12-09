module Hey.Extra.CustomEvent (detail, customEvent) where

import Data.Newtype (unwrap)
import Foreign (Foreign, unsafeToForeign)
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.Event (EventType)

foreign import detail :: CustomEvent -> Foreign

foreign import customEvent_ :: String -> Foreign -> CustomEvent

customEvent :: forall a. EventType -> a -> CustomEvent
customEvent etype a = customEvent_ (unwrap etype) (unsafeToForeign a)
