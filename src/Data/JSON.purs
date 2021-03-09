module Hey.Data.JSON where

import Prelude
import Data.Argonaut (Json, JsonDecodeError(..))
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.DateTime (DateTime(..), Time(..), canonicalDate)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe)

type DateTimeParts
  = { year :: Int
    , month :: Int
    , day :: Int
    , hour :: Int
    , minute :: Int
    , second :: Int
    , millisecond :: Int
    }

foreign import parseDateTime :: Json -> Nullable DateTimeParts

newtype JDateTime
  = JDateTime DateTime

derive instance newtypeJDateTime :: Newtype JDateTime _

instance decodeJDateTime :: DecodeJson JDateTime where
  decodeJson json =
    (parseDateTime >>> toMaybe >=> build)
      >>> maybe (Left $ UnexpectedValue json) (JDateTime >>> Right)
      $ json
    where
    build :: DateTimeParts -> Maybe DateTime
    build d = DateTime <$> buildDate d <*> buildTime d

    buildDate { year, month, day } = do
      year' <- toEnum year
      month' <- toEnum month
      day' <- toEnum day
      pure $ canonicalDate year' month' day'

    buildTime { hour, minute, second, millisecond } = do
      hour' <- toEnum hour
      minute' <- toEnum minute
      second' <- toEnum second
      millisecond' <- toEnum millisecond
      pure $ Time hour' minute' second' millisecond'
