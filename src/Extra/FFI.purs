module Hey.Extra.FFI where

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import instanceOf :: String -> Foreign -> Boolean

unsafeInstanceCoerce :: forall a. String -> Foreign -> Maybe a
unsafeInstanceCoerce name f =
  if instanceOf name f then
    Just (unsafeCoerce f)
  else
    Nothing
