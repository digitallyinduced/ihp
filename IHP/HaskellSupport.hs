{-# LANGUAGE TypeFamilies, DataKinds, MultiParamTypeClasses, PolyKinds, TypeApplications, ScopedTypeVariables, TypeInType, ConstraintKinds, TypeOperators, GADTs, UndecidableInstances, StandaloneDeriving, IncoherentInstances, AllowAmbiguousTypes, FunctionalDependencies #-}

{-|
Module: IHP.HaskellSupport
Description: Provides helpers to write better haskell code
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.HaskellSupport (
 (|>)
, isEmpty
, whenEmpty
, whenNonEmpty
, get
, set
, ifOrEmpty
, modify
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
, IsEmpty (..)
) where

import ClassyPrelude
import Control.Monad (when)
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
import qualified Data.Maybe 

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

instance forall name name'. (KnownSymbol name, name' ~ name) => IsLabel name (Proxy name') where
    fromLabel = Proxy @name'

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

{-# INLINE modify #-}
modify :: forall model name value updateFunction. (KnownSymbol name, Record.HasField name model value, SetField name model value) => Proxy name -> (value -> value) -> model -> model
modify _ updateFunction model = let value = Record.getField @name model in setField @name (updateFunction value) model

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


-- | Example:
-- forEach users \user -> putStrLn (tshow user)
forEach :: (MonoFoldable mono, Applicative m) => mono -> (Element mono -> m ()) -> m ()
forEach elements function = forM_ elements function
{-# INLINE forEach #-}


-- | Example:
-- forEachWithIndex users \(index, user) -> putStrLn (tshow user)
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

instance IsString UUID.UUID where
    fromString string = case UUID.fromString string of
            Just uuid -> uuid
            Nothing -> error ("Invalid UUID: " <> string)