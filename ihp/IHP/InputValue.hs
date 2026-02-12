module IHP.InputValue
( InputValue (..)
) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time.Calendar (Day)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import Data.Scientific (Scientific)
import Data.String.Conversions (cs)
import qualified Numeric
import PostgresqlTypes.Interval (Interval)
import PostgresqlTypes.Inet (Inet)

-- | Provides a way to convert a Haskell value to a Text representation
-- for use in HTML @\<input value="..."\/>@ attributes.
--
-- This is used by IHP's form helpers to populate form field values.
--
-- __Example:__
--
-- >>> inputValue True
-- "on"
--
-- >>> inputValue (1 :: Int)
-- "1"
--
class InputValue a where
    inputValue :: a -> Text

instance InputValue Text where
    inputValue text = text

instance InputValue Int where
    inputValue = Text.pack . show

instance InputValue Integer where
    inputValue = Text.pack . show

instance InputValue Double where
    inputValue double = cs (Numeric.showFFloat Nothing double "")

instance InputValue Float where
    inputValue float = cs (Numeric.showFFloat Nothing float "")

instance InputValue Bool where
    inputValue True = "on"
    inputValue False = "off"

instance InputValue UUID where
    inputValue = UUID.toText

instance InputValue () where
    inputValue () = "error: inputValue(()) not supported"

instance InputValue UTCTime where
    inputValue time = cs (iso8601Show time)

instance InputValue LocalTime where
    inputValue time = cs (iso8601Show time)

instance InputValue Day where
    inputValue date = cs (iso8601Show date)

instance InputValue TimeOfDay where
    inputValue timeOfDay = Text.pack (show timeOfDay)

instance InputValue Interval where
    inputValue interval = Text.pack (show interval)

instance InputValue Inet where
    inputValue inet = Text.pack (show inet)

instance InputValue fieldType => InputValue (Maybe fieldType) where
    inputValue (Just value) = inputValue value
    inputValue Nothing = ""

instance InputValue value => InputValue [value] where
    inputValue list = Text.intercalate "," (map inputValue list)

instance InputValue Value where
    inputValue json = cs (Aeson.encode json)

instance InputValue Scientific where
    inputValue = Text.pack . show
