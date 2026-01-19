{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeOperators, GADTs, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes, FunctionalDependencies #-}

{-|
Module: IHP.HaskellSupport
Description: Provides helpers to write better haskell code
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.HaskellSupport
( (|>)
, (|>>)
, whenEmpty
, whenNonEmpty
, get
, set
, setJust
, setMaybe
, ifOrEmpty
, modify
, modifyJust
, SetField (..)
, UpdateField (..)
, incrementField
, decrementField
, isToday
, isToday'
, forEach
, forEachWithIndex
, textToInt
, isWeekend
, todayIsWeekend
, debug
, includes
, stripTags
, symbolToText
, symbolToByteString
, IsEmpty (..)
, copyFields
, allEnumValues
) where

import Control.Monad (when, unless, forM_)
import Data.Functor ((<&>))
import Data.String (IsString(..))
import Data.MonoTraversable (MonoFoldable, Element, oelem, oforM_)
import Data.Time.Clock (UTCTime(..), getCurrentTime)
import Data.Time.Calendar (Day, toGregorian)
import Data.Map (Map)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Prelude
import qualified Data.Default
import qualified Data.UUID as UUID
import Data.Proxy
import qualified Data.Time
import GHC.TypeLits
import GHC.OverloadedLabels
import qualified GHC.Records as Record
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.String.Conversions (cs, ConvertibleStrings (..))
import qualified Debug.Trace
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Aeson.Key as Aeson

import IHP.Record ((|>), get, set, setJust, setMaybe, modify, modifyJust, SetField(..), UpdateField(..), incrementField, decrementField, copyFields, CopyFields(..))

infixl 8 |>>
a |>> b = a <&> b
{-# INLINABLE (|>>) #-}

-- | Used by 'nonEmpty' and 'isEmptyValue' to check for emptyness
class IsEmpty value where
    -- | Returns True when the value is an empty string, empty list, zero UUID, etc.
    isEmpty :: value -> Bool

instance IsEmpty Text where
    isEmpty "" = True
    isEmpty _ = False
    {-# INLINE isEmpty #-}

instance IsEmpty (Maybe value) where
    isEmpty Nothing = True
    isEmpty (Just _) = False
    {-# INLINE isEmpty #-}

instance IsEmpty [a] where
    isEmpty [] = True
    isEmpty _ = False
    {-# INLINE isEmpty #-}

instance IsEmpty UUID.UUID where
    isEmpty uuid = UUID.nil == uuid
    {-# INLINE isEmpty #-}

instance IsEmpty (Map a b) where
    isEmpty = Map.null    
    {-# INLINE isEmpty #-}

ifOrEmpty :: (Monoid a) => Bool -> a -> a
ifOrEmpty bool a = if bool then a else mempty
{-# INLINE ifOrEmpty #-}

whenEmpty condition = when (isEmpty condition)
{-# INLINE whenEmpty #-}

whenNonEmpty :: (IsEmpty a, Applicative f) => a -> f () -> f ()
whenNonEmpty condition = unless (isEmpty condition)
{-# INLINE whenNonEmpty #-}

-- Returns 'True' when a value is contained in the given list, array, set, ...
--
-- Alias for 'elem', but with a nicer name :)
--
-- >>> ["hello", "world"] |> includes "hello"
-- True
--
-- >>> "Hello" |> includes 'H'
-- True
includes :: (MonoFoldable container, Eq (Element container)) => Element container -> container -> Bool
includes = oelem
{-# INLINE includes #-}

instance Data.Default.Default UUID.UUID where
    def = UUID.nil
    {-# INLINE def #-}

instance forall name name'. (KnownSymbol name, name' ~ name) => IsLabel name (Proxy name') where
    fromLabel = Proxy @name'
    {-# INLINE fromLabel #-}

utcTimeToYearMonthDay :: UTCTime -> (Integer, Int, Int)
utcTimeToYearMonthDay = toGregorian . utctDay -- (year,month,day)

isToday :: UTCTime -> IO Bool
isToday timestamp = do
    now <- getCurrentTime
    pure (isToday' now timestamp)

isToday' :: UTCTime -> UTCTime -> Bool
isToday' currentTime timestamp = utcTimeToYearMonthDay currentTime == utcTimeToYearMonthDay timestamp

-- | Allows `Just "someThing"` to be written as `"someThing"`
instance IsString string => IsString (Maybe string) where
    fromString string = Just (fromString string)
    {-# INLINE fromString #-}


-- | Example:
--
-- > forEach users \user -> putStrLn (tshow user)
--
-- __Example:__ Within HSX
--
-- > renderUser :: User -> Html
-- > renderUser user = [hsx|<div>User: {user.name}</div>|]
-- >
-- > render = [hsx|{forEach users renderUser}|]
--
forEach :: (MonoFoldable mono, Applicative m) => mono -> (Element mono -> m ()) -> m ()
forEach elements function = oforM_ elements function
{-# INLINE forEach #-}


-- | Like 'forEach' but with an index, starting at 0
--
-- __Example:__ With a Callback
--
-- > forEachWithIndex users \(index, user) -> putStrLn (tshow index <> ": " <> tshow user)
--
-- __Example:__ With a Function
--
-- > printUser :: (Int, User) -> IO ()
-- > printUser (index, user) = putStrLn (tshow index <> ": " <> tshow user)
-- >
-- > forEachWithIndex users printUser
--
-- __Example:__ Within HSX
--
-- > renderUser :: (Int, User) -> Html
-- > renderUser (index, user) = [hsx|<div>User {index}: {user.name}</div>|]
-- >
-- > render = [hsx|{forEachWithIndex users renderUser}|]
--
forEachWithIndex :: (Monad m) => [a] -> ((Int, a) -> m ()) -> m ()
forEachWithIndex elements function = forM_ (zip [0..] elements) function
{-# INLINE forEachWithIndex #-}

-- | Parses a text to an int. Returns @Nothing@ on failure.
--
-- __Example:__
--
-- >>> textToInt "1337"
-- Just 1337
--
-- >>> textToInt "bad input"
-- Nothing
textToInt :: Text -> Maybe Int
textToInt text = case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) (cs text) of
    Right value -> Just value
    Left _error -> Nothing

-- | Returns @True@ when today is Saturday or Sunday.
--
-- __Example:__
--
-- > do
-- >     todayIsWeekend <- isWeekend
-- >     when todayIsWeekend (putStrLn "It's weekend!")
todayIsWeekend :: IO Bool
todayIsWeekend = do
    now <- Data.Time.getCurrentTime
    let today = Data.Time.utctDay now
    return (isWeekend today)

-- | Returns @True@ when day is Saturday or Sunday.
--
-- __Example:__
--
-- >>> isWeekend $ fromGregorian 2019 10 7
-- False
--
-- >>> isWeekend $ fromGregorian 2020 6 13
-- True
isWeekend :: Day -> Bool
isWeekend day =
  weekday == Data.Time.Saturday || weekday == Data.Time.Sunday
  where
    weekday = Data.Time.dayOfWeek day

-- | Debug-print a value during evaluation
--
-- Alias for 'Debug.Trace.traceShowId'
debug :: Show value => value -> value
debug value = Debug.Trace.traceShowId value
{-# INLINE debug #-}

-- | Removes all html tags from a given html text
--
-- >>> stripTags "This is <b>Bold</b>"
-- "This is Bold"
stripTags :: Text -> Text
stripTags "" = ""
stripTags html | Text.head html == '<' = stripTags (Text.drop 1 (Text.dropWhile (/= '>') (Text.tail html)))
stripTags html = let (a, b) = Text.splitAt 1 html in a <> stripTags b

-- | Returns the value of a type level symbol as a text
--
-- >>> symbolToText @"hello"
-- "hello"
--
-- >>> symbolToText @(GetTableName User)
-- "users"
symbolToText :: forall symbol. (KnownSymbol symbol) => Text
symbolToText = Text.pack (symbolVal @symbol Proxy)
{-# INLINE symbolToText #-}

-- | Returns the value of a type level symbol as a bytestring
--
-- >>> symbolToByteString @"hello"
-- "hello"
--
-- >>> symbolToByteString @(GetTableName User)
-- "users"
symbolToByteString :: forall symbol. (KnownSymbol symbol) => ByteString
symbolToByteString = ByteString.pack (symbolVal @symbol Proxy)
{-# INLINE symbolToByteString #-}

instance IsString UUID.UUID where
    fromString string = case UUID.fromString string of
            Just uuid -> uuid
            Nothing -> error ("Invalid UUID: " <> string)

-- | Returns a list of all values of an enum type
--
-- Given a data structure like this:
--
-- > data Color = Yellow | Red | Blue deriving (Enum)
--
-- You can call 'allEnumValues' to get a list of all colors:
--
-- >>> allEnumValues @Color
-- [Yellow, Red, Blue]
--
-- This also works if the enum is defined in the @Schema.sql@:
--
-- > CREATE TYPE brokerage_subscription_type AS ENUM ('basic_subscription', 'bronze_subscription', 'silver_subscription', 'gold_subscription');
--
-- >>> allEnumValues @BrokerageSubscriptionType
-- [BasicSubscription, BronzeSubscription, SilverSubscription]
--
allEnumValues :: forall enumType. Enum enumType => [enumType]
allEnumValues = enumFrom (toEnum 0)
{-# INLINABLE allEnumValues #-}

instance ConvertibleStrings ByteString Aeson.Key where
    convertString byteString = Aeson.fromText (cs byteString)
instance ConvertibleStrings Text Aeson.Key where
    convertString text = Aeson.fromText text
