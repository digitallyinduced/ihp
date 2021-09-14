{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes, FunctionalDependencies #-}

{-|
Module: IHP.HaskellSupport
Description: Provides helpers to write better haskell code
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.HaskellSupport
( (|>)
, whenEmpty
, whenNonEmpty
, get
, set
, setJust
, setIfJust
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

import ClassyPrelude
import qualified Data.Default
import qualified Data.UUID as UUID
import Data.Proxy
import qualified Data.Time
import GHC.TypeLits
import GHC.OverloadedLabels
import qualified GHC.Records as Record
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import Data.String.Conversions (cs)
import qualified Debug.Trace
import qualified Data.Text as Text
import qualified Data.ByteString.Char8 as ByteString

--(|>) :: a -> f -> f a
infixl 8 |>
a |> f = f a
{-# INLINE (|>) #-}

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
includes = elem
{-# INLINE includes #-}

instance Data.Default.Default UUID.UUID where
    def = UUID.nil
    {-# INLINE def #-}

instance forall name name'. (KnownSymbol name, name' ~ name) => IsLabel name (Proxy name') where
    fromLabel = Proxy @name'
    {-# INLINE fromLabel #-}

-- | Returns the field value for a field name
--
-- __Example:__
-- 
-- > data Project = Project { name :: Text, isPublic :: Bool }
-- > 
-- > let project = Project { name = "Hello World", isPublic = False }
--
-- >>> get #name project
-- "Hello World"
--
-- >>> get #isPublic project
-- False
get :: forall model name value. (KnownSymbol name, Record.HasField name model value) => Proxy name -> model -> value
get _ record = Record.getField @name record
{-# INLINE get #-}

-- | Sets a field of a record and returns the new record.
--
-- __Example:__
-- 
-- > data Project = Project { name :: Text, isPublic :: Bool }
-- > 
-- > let project = Project { name = "Hello World", isPublic = False }
--
-- >>> set #name "New Name" project
-- Project { name = "New Name", isPublic = False }
--
-- >>> set #isPublic True project
-- Project { name = "Hello World", isPublic = True }
set :: forall model name value. (KnownSymbol name, SetField name model value) => Proxy name -> value -> model -> model
set name value record = setField @name value record
{-# INLINE set #-}


-- | Like 'set' but doesn't set the value if it's 'Nothing'. Useful when you update NULL values
-- | e.g. via a cron job and don't want to lose that work on subsequent updates.
--
-- __Example:__
--
-- > data Project = Project { name :: Maybe Text }
-- >
-- > let project = Project { name = Nothing }
--
-- >>> setIfJust #name (Just "New Name") project
-- Project { name = Just "New Name" }
--
-- >>> setIfJust #name Nothing project
-- Project { name = Just "New Name" } -- previous value is kept
--
setIfJust :: forall model name value. (KnownSymbol name, SetField name model (Maybe value)) => Proxy name -> Maybe value -> model -> model
setIfJust name value record = case value of
    Just value -> setField @name (Just value) record
    Nothing    -> record
{-# INLINE setIfJust #-}


-- | Like 'set' but wraps the value with a 'Just'. Useful when you want to set a 'Maybe' field
--
-- __Example:__
-- 
-- > data Project = Project { name :: Maybe Text }
-- > 
-- > let project = Project { name = Nothing }
--
-- >>> setJust #name "New Name" project
-- Project { name = Just "New Name" }
--
setJust :: forall model name value. (KnownSymbol name, SetField name model (Maybe value)) => Proxy name -> value -> model -> model
setJust name value record = setField @name (Just value) record
{-# INLINE setJust #-}


modify :: forall model name value updateFunction. (KnownSymbol name, Record.HasField name model value, SetField name model value) => Proxy name -> (value -> value) -> model -> model
modify _ updateFunction model = let value = Record.getField @name model in setField @name (updateFunction value) model
{-# INLINE modify #-}

-- Like 'modify', but only modifies the value if it's not Nothing.
--
-- __Example:__
--
-- > let pauseDuration = now `diffUTCTime` pausedAt
-- >
-- > floorTimer <- floorTimer
-- >         |> modifyJust #startedAt (addUTCTime pauseDuration)
-- >         |> updateRecord
--
modifyJust :: forall model name value updateFunction. (KnownSymbol name, Record.HasField name model (Maybe value), SetField name model (Maybe value)) => Proxy name -> (value -> value) -> model -> model
modifyJust _ updateFunction model = case Record.getField @name model of
        Just value -> setField @name (Just (updateFunction value)) model
        Nothing -> model
{-# INLINE modifyJust #-}

-- | Plus @1@ on record field.
--
-- __Example:__
-- 
-- > data Project = Project { name :: Text, followersCount :: Int }
-- > 
-- > let project = Project { name = "Hello World", followersCount = 0 }
--
-- >>> project |> incrementField #followersCount
-- Project { name = "Hello World", followersCount = 1 }
incrementField :: forall model name value. (KnownSymbol name, Record.HasField name model value, SetField name model value, Num value) => Proxy name -> model -> model
incrementField _ model = let value = Record.getField @name model in setField @name (value + 1) model
{-# INLINE incrementField #-}

-- | Minus @1@ on a record field.
--
-- __Example:__
-- 
-- > data Project = Project { name :: Text, followersCount :: Int }
-- > 
-- > let project = Project { name = "Hello World", followersCount = 1337 }
--
-- >>> project |> decrementField #followersCount
-- Project { name = "Hello World", followersCount = 1336 }
decrementField :: forall model name value. (KnownSymbol name, Record.HasField name model value, SetField name model value, Num value) => Proxy name -> model -> model
decrementField _ model = let value = Record.getField @name model in setField @name (value - 1) model
{-# INLINE decrementField #-}

class SetField (field :: GHC.TypeLits.Symbol) model value | field model -> value where
    setField :: value -> model -> model

class Record.HasField field model value => UpdateField (field :: GHC.TypeLits.Symbol) model model' value value' | model model' value' -> value where
    updateField :: value' -> model -> model'

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
-- > renderUser user = [hsx|<div>User: {get #name user}</div>|]
-- >
-- > render = [hsx|{forEach users renderUser}|]
--
forEach :: (MonoFoldable mono, Applicative m) => mono -> (Element mono -> m ()) -> m ()
forEach elements function = forM_ elements function
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
-- > renderUser (index, user) = [hsx|<div>User {index}: {get #name user}</div>|]
-- >
-- > render = [hsx|{forEachWithIndex users renderUser}|]
--
forEachWithIndex :: (Applicative m) => [a] -> ((Int, a) -> m ()) -> m ()
forEachWithIndex elements function = forM_ (ClassyPrelude.zip [0..] elements) function
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


class CopyFields (fields :: [Symbol]) destinationRecord sourceRecord where
    -- | Provides the 'copyFields' function
    --
    -- Useful to rewrite getter-setter code like this:
    --
    -- > let newProject = newRecord @Project
    -- >     |> set #name (get #name otherProject)
    -- >     |> set #isPublic (get #isPublic otherProject)
    -- >     |> set #userId (get #userId otherProject)
    --
    -- With 'copyFields' this can be written like this:
    --
    -- > let newProject = newRecord @Project
    -- >     |> copyFields @["name", "isPublic", "userId"] otherProject
    --
    copyFields :: sourceRecord -> destinationRecord -> destinationRecord

instance CopyFields ('[]) destinationRecord sourceRecord where
    copyFields sourceRecord destinationRecord = destinationRecord
    {-# INLINE copyFields #-}

instance (CopyFields rest destinationRecord sourceRecord
    , KnownSymbol fieldName
    , SetField fieldName destinationRecord fieldType
    , Record.HasField fieldName sourceRecord fieldType
    ) => CopyFields (fieldName:rest) destinationRecord sourceRecord where
    copyFields sourceRecord destinationRecord =
            destinationRecord
            |> set (Proxy @fieldName) (Record.getField @fieldName sourceRecord)
            |> copyFields @rest sourceRecord
    {-# INLINE copyFields #-}

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