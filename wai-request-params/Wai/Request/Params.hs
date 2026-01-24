{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, BlockArguments, DataKinds #-}

{-|
Module: Wai.Request.Params
Description: Generic parameter parsing for WAI requests
Copyright: (c) digitally induced GmbH, 2020

This module provides generic parameter parsing functionality for WAI requests,
supporting both form-encoded and JSON request bodies.

All functions take explicit 'RequestBody' and 'Request' parameters, making this
module suitable for use outside of IHP.
-}
module Wai.Request.Params
( -- * Reading parameters
  param
, paramOrNothing
, paramOrDefault
, paramOrError
, paramList
, paramListOrNothing
, hasParam
, queryOrBodyParam
, allParams
  -- * ParamReader typeclass
, ParamReader (..)
  -- * Exceptions
, ParamException (..)
  -- * Helper functions for custom ParamReader instances
, enumParamReader
, enumParamReaderJSON
  -- * Re-exports from Middleware
, RequestBody (..)
, requestBodyVaultKey
) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (LocalTime, TimeOfDay)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified GHC.Float as Float
import qualified Control.Exception as Exception
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Scientific as Scientific
import qualified Data.Vector as Vector
import qualified Control.DeepSeq as DeepSeq
import Text.Read (readMaybe)
import qualified Data.Either as Either
import qualified Network.Wai as Wai
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import Data.Char (toLower)
import Data.String.Conversions (cs)
import GHC.TypeLits (TypeError, ErrorMessage (Text), Symbol)

import Wai.Request.Params.Middleware (RequestBody (..), requestBodyVaultKey)

-- | Returns a query or body parameter from a request. The raw string
-- value is parsed before returning it. So the return value type depends on what
-- you expect (e.g. can be Int, Text, UUID, Bool, some custom type).
--
-- When the parameter is missing or cannot be parsed, an exception is thrown.
-- Use 'paramOrDefault' when you want to get a default value instead of an exception,
-- or 'paramOrNothing' to get @Nothing@ when the parameter is missing.
--
-- You can define a custom parameter parser by defining a 'ParamReader' instance.
--
-- __Example:__
--
-- > let maxItems :: Int = param requestBody request "maxItems"
--
param :: ParamReader valueType => RequestBody -> Wai.Request -> ByteString -> valueType
param !requestBody !request !name = case paramOrError requestBody request name of
        Left exception -> Exception.throw exception
        Right value -> value
{-# INLINABLE param #-}

-- | Similar to 'param' but works with multiple params. Useful when working with checkboxes.
--
-- Given a query like:
--
-- > ingredients=milk&ingredients=egg
--
-- This will return @["milk", "egg"]@ for @paramList requestBody request "ingredients"@
--
-- When no parameter with the name is given, an empty list is returned.
-- When a value cannot be parsed, this function will fail similar to 'param'.
paramList :: forall valueType. (DeepSeq.NFData valueType, ParamReader valueType) => RequestBody -> Wai.Request -> ByteString -> [valueType]
paramList requestBody request name =
    allParams requestBody request
    |> filter (\(paramName, _) -> paramName == name)
    |> mapMaybe (\(_, paramValue) -> paramValue)
    |> map (readParameter @valueType)
    |> map (Either.fromRight (error (paramParserErrorMessage name)))
    |> DeepSeq.force
{-# INLINABLE paramList #-}

-- | Similar to 'paramOrNothing' but works with multiple params. This is useful when submitting multiple
-- input fields with the same name, and some may be empty.
--
-- Given a query like (note the @ingredients@ in the middle that has no value):
--
-- > ingredients=milk&ingredients&ingredients=egg
--
-- This will return @[Just "milk", Nothing, Just "egg"]@
--
-- When no parameter with the name is given, an empty list is returned.
paramListOrNothing :: forall valueType. (DeepSeq.NFData valueType, ParamReader valueType) => RequestBody -> Wai.Request -> ByteString -> [Maybe valueType]
paramListOrNothing requestBody request name =
    allParams requestBody request
    |> filter (\(paramName, _) -> paramName == name)
    |> mapMaybe (\(_, paramValue) -> paramValue)
    |> map (\paramValue -> if paramValue == "" then Left "Empty ByteString" else readParameter @valueType paramValue)
    |> map (\value -> case value of
            Left _ -> Nothing
            Right val -> Just val
        )
    |> DeepSeq.force
{-# INLINABLE paramListOrNothing #-}

paramParserErrorMessage :: ByteString -> String
paramParserErrorMessage name = "param: Parameter '" <> cs name <> "' is invalid"

-- | Thrown when a parameter is missing when calling 'param' or related functions
data ParamException
    = ParamNotFoundException { name :: ByteString }
    | ParamCouldNotBeParsedException { name :: ByteString, parserError :: ByteString }
    deriving (Show, Eq)

instance Exception.Exception ParamException where
    displayException (ParamNotFoundException { name }) = "param: Parameter '" <> cs name <> "' not found"
    displayException (ParamCouldNotBeParsedException { name, parserError }) = "param: Parameter '" <> cs name <> "' could not be parsed, " <> cs parserError

-- | Returns @True@ when a parameter is given in the request via the query or request body.
--
-- Use 'paramOrDefault' when you want to use this for providing a default value.
hasParam :: RequestBody -> Wai.Request -> ByteString -> Bool
hasParam requestBody request = isJust . queryOrBodyParam requestBody request
{-# INLINABLE hasParam #-}

-- | Like 'param', but returns a default value when the parameter is missing instead of throwing
-- an exception.
--
-- Use 'paramOrNothing' when you want to get @Maybe@.
paramOrDefault :: ParamReader a => RequestBody -> Wai.Request -> a -> ByteString -> a
paramOrDefault requestBody request !defaultValue = fromMaybe defaultValue . paramOrNothing requestBody request
{-# INLINABLE paramOrDefault #-}

-- | Like 'param', but returns @Nothing@ when the parameter is missing instead of throwing
-- an exception.
--
-- Use 'paramOrDefault' when you want to deal with a default value.
paramOrNothing :: forall paramType. ParamReader (Maybe paramType) => RequestBody -> Wai.Request -> ByteString -> Maybe paramType
paramOrNothing requestBody request !name =
    case paramOrError requestBody request name of
        Left ParamNotFoundException {} -> Nothing
        Left otherException -> Exception.throw otherException
        Right value -> value
{-# INLINABLE paramOrNothing #-}

-- | Like 'param', but returns @Left "Some error message"@ if the parameter is missing or invalid
paramOrError :: forall paramType. ParamReader paramType => RequestBody -> Wai.Request -> ByteString -> Either ParamException paramType
paramOrError !requestBody !request !name =
    case requestBody of
        FormBody {} -> case queryOrBodyParam requestBody request name of
                Just value -> case readParameter @paramType value of
                    Left parserError -> Left ParamCouldNotBeParsedException { name, parserError }
                    Right value' -> Right value'
                Nothing -> Left ParamNotFoundException { name }
        JSONBody { jsonPayload } -> case jsonPayload of
                (Just (Aeson.Object hashMap)) -> case Aeson.lookup (Aeson.fromText $ cs name) hashMap of
                    Just value -> case readParameterJSON @paramType value of
                        Left parserError -> Left ParamCouldNotBeParsedException { name, parserError }
                        Right value' -> Right value'
                    Nothing -> Left ParamNotFoundException { name }
                _ -> Left ParamNotFoundException { name }
{-# INLINABLE paramOrError #-}

-- | Returns a parameter without any parsing. Returns @Nothing@ when the parameter is missing.
queryOrBodyParam :: RequestBody -> Wai.Request -> ByteString -> Maybe ByteString
queryOrBodyParam requestBody request !name = join (lookup name (allParams requestBody request))
{-# INLINABLE queryOrBodyParam #-}

-- | Returns all params available in the request
allParams :: RequestBody -> Wai.Request -> [(ByteString, Maybe ByteString)]
allParams requestBody request = case requestBody of
            FormBody { params, files } -> concat [(map (\(a, b) -> (a, Just b)) params), (Wai.queryString request)]
            JSONBody { jsonPayload } -> error "allParams: Not supported for JSON requests"

-- | Input parser for 'param'.
--
-- Parses the input bytestring. Returns @Left "some error"@ when there is an error parsing the value.
-- Returns @Right value@ when the parsing succeeded.
class ParamReader a where
    -- | The error messages here should be human-readable, as they're visible e.g. in forms
    readParameter :: ByteString -> Either ByteString a
    -- | The error messages here are directed at other developers, so they can be a bit more technical than 'readParameter' errors
    readParameterJSON :: Aeson.Value -> Either ByteString a
    readParameterJSON = enumParamReaderJSON

instance ParamReader ByteString where
    {-# INLINABLE readParameter #-}
    readParameter byteString = pure byteString

    readParameterJSON (Aeson.String bytestring) = Right (cs bytestring)
    readParameterJSON _ = Left "Expected String"

instance ParamReader Int where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly ((Attoparsec.signed Attoparsec.decimal) <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left _ -> Left "has to be an integer"

    readParameterJSON (Aeson.Number number) =
            case Scientific.floatingOrInteger number of
                    Left _ -> Left "Expected Int"
                    Right int -> Right int
    readParameterJSON _ = Left "Expected Int"

instance ParamReader Integer where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly ((Attoparsec.signed Attoparsec.decimal) <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left _ -> Left "has to be an integer"

    readParameterJSON (Aeson.Number number) =
            case Scientific.floatingOrInteger number of
                    Left _ -> Left "Expected Integer"
                    Right integer -> Right integer
    readParameterJSON _ = Left "Expected Integer"

instance ParamReader Double where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.double <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left _ -> Left "has to be a number with decimals"

    readParameterJSON (Aeson.Number number) =
            case Scientific.floatingOrInteger number of
                    Left double -> Right double
                    Right integer -> Right (fromIntegral integer)
    readParameterJSON _ = Left "Expected Double"

instance ParamReader Scientific.Scientific where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.scientific <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left _ -> Left "has to be a number with decimals"

    readParameterJSON (Aeson.Number number) = Right number
    readParameterJSON _ = Left "Expected Scientific"

instance ParamReader Float where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.double <* Attoparsec.endOfInput) byteString of
            Right value -> Right (Float.double2Float value)
            Left _ -> Left "has to be a number with decimals"

    readParameterJSON (Aeson.Number number) =
            case Scientific.floatingOrInteger number of
                    Left double -> Right double
                    Right integer -> Right (fromIntegral integer)
    readParameterJSON _ = Left "Expected Float"

instance ParamReader Text where
    {-# INLINABLE readParameter #-}
    readParameter byteString = pure (cs byteString)

    readParameterJSON (Aeson.String text) = Right text
    readParameterJSON _ = Left "Expected String"

-- | Parses comma separated input like @userIds=1,2,3@
--
-- __Example:__
--
-- >>> readParameter @[Int] "1,2,3"
-- Right [1,2,3]
instance ParamReader value => ParamReader [value] where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        byteString
        |> Char8.split ','
        |> map readParameter
        |> Either.partitionEithers
        |> \case
            ([], values) -> Right values
            ((first:_), _) -> Left first

    readParameterJSON (Aeson.Array values) =
        values
        |> Vector.toList
        |> map readParameterJSON
        |> Either.partitionEithers
        |> \case
            ([], values') -> Right values'
            ((first:_), _) -> Left first
    readParameterJSON _ = Left "Expected Array"

-- | Parses a boolean.
--
-- Html form checkboxes usually use @on@ or @off@ for representation. These
-- values are supported here.
--
-- - @"on"@ maps to @True@ (HTML checkbox default)
-- - @"true"@ (case-insensitive) maps to @True@
-- - Everything else maps to @False@
instance ParamReader Bool where
    {-# INLINABLE readParameter #-}
    readParameter value
        | value == "on" = pure True
        | map toLower (cs value) == "true" = pure True
        | otherwise = pure False

    readParameterJSON (Aeson.Bool bool) = Right bool
    readParameterJSON _ = Left "Expected Bool"

instance ParamReader UUID where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case UUID.fromASCIIBytes byteString of
            Just uuid -> pure uuid
            Nothing -> Left "has to be an UUID"

    readParameterJSON (Aeson.String string) =
        case UUID.fromText string of
            Just uuid -> pure uuid
            Nothing -> Left "Invalid UUID"
    readParameterJSON _ = Left "Expected String with an UUID"

-- | Accepts values such as @2020-11-08T12:03:35Z@ or @2020-11-08@
instance ParamReader UTCTime where
    {-# INLINABLE readParameter #-}
    readParameter "" = Left "This field cannot be empty"
    readParameter byteString =
        let
            input = (cs byteString)
            dateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" input
            date = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" input
        in case dateTime of
            Nothing -> case date of
                Just value -> Right value
                Nothing -> Left "has to be a valid date and time, e.g. 2020-11-08T12:03:35Z"
            Just value -> Right value

    readParameterJSON (Aeson.String string) = readParameter (cs string)
    readParameterJSON _ = Left "Expected String"

-- | Accepts values such as @2020-11-08T12:03:35Z@ or @2020-11-08@
instance ParamReader LocalTime where
    {-# INLINABLE readParameter #-}
    readParameter "" = Left "This field cannot be empty"
    readParameter byteString =
        let
            input = (cs byteString)
            dateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" input
            date = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" input
        in case dateTime of
            Nothing -> case date of
                Just value -> Right value
                Nothing -> Left "has to be a valid date and time, e.g. 2020-11-08T12:03:35Z"
            Just value -> Right value

    readParameterJSON (Aeson.String string) = readParameter (cs string)
    readParameterJSON _ = Left "Expected String"

-- | Accepts values such as @2020-11-08@
instance ParamReader Day where
    {-# INLINABLE readParameter #-}
    readParameter "" = Left "This field cannot be empty"
    readParameter byteString =
        let
            input = (cs byteString)
            date = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" input
        in case date of
            Just value -> Right value
            Nothing -> Left "has to be a date, e.g. 2020-11-08"

    readParameterJSON (Aeson.String string) = readParameter (cs string)
    readParameterJSON _ = Left "Expected String"

instance ParamReader TimeOfDay where
    {-# INLINABLE readParameter #-}
    readParameter "" = Left "This field cannot be empty"
    readParameter byteString =
        let
            input = (cs byteString)
        in case readMaybe input of
            Just value -> Right value
            Nothing -> Left "has to be time in the format hh:mm:ss"

    readParameterJSON (Aeson.String string) = readParameter (cs string)
    readParameterJSON _ = Left "Expected String"

instance ParamReader param => ParamReader (Maybe param) where
    {-# INLINABLE readParameter #-}
    readParameter paramValue =
        case (readParameter paramValue) :: Either ByteString param of
            Right value -> Right (Just value)
            Left _ | paramValue == "" -> Right Nothing
            Left err -> Left err

    readParameterJSON value =
        case (readParameterJSON value) :: Either ByteString param of
            Right value' -> Right (Just value')
            Left _ | value == (Aeson.String "") -> Right Nothing
            Left err -> Left err

-- | Custom error hint when the 'param' is called with do-notation
--
-- __Example:__
--
-- > action Example = do
-- >     myParam <- param requestBody request "hello"
--
-- Now a custom type error will be shown telling the user to use @let myParam = param ...@ instead of do-notation.
instance (TypeError ('Text ("Use 'let x = param requestBody request \"..\"' instead of 'x <- param requestBody request \"..\"'" :: Symbol))) => ParamReader (IO param) where
    readParameter _ = error "Unreachable"
    readParameterJSON _ = error "Unreachable"

-- | Can be used as a default implementation for 'readParameter' for enum structures
--
-- __Example:__
--
-- > data Color = Yellow | Red | Blue deriving (Enum)
-- >
-- > instance ParamReader Color where
-- >     readParameter = enumParamReader @Color inputValueForColor
-- >     readParameterJSON = enumParamReaderJSON
-- >
-- > inputValueForColor :: Color -> Text
-- > inputValueForColor = Text.pack . show
--
-- Note: This requires providing an @inputValue@ function to convert enum values to Text.
enumParamReader :: forall parameter. (Enum parameter, Bounded parameter) => (parameter -> Text) -> ByteString -> Either ByteString parameter
enumParamReader toText string =
        case filter (\value -> toText value == string') allEnumValues of
            (value:_) -> Right value
            [] -> Left "Invalid value"
    where
        string' = cs string

-- | Used as a default implementation for 'readParameterJSON'
--
-- __Example:__
--
-- > data Color = Yellow | Red | Blue deriving (Enum)
-- >
-- > instance ParamReader Color where
-- >     readParameter = enumParamReader
-- >     readParameterJSON = enumParamReaderJSON
enumParamReaderJSON :: forall parameter. (ParamReader parameter) => Aeson.Value -> Either ByteString parameter
enumParamReaderJSON (Aeson.String string) = readParameter (cs string)
enumParamReaderJSON _ = Left "enumParamReaderJSON: Invalid value, expected a string but got something else"

-- Helpers

(|>) :: a -> (a -> b) -> b
(|>) a f = f a
infixl 1 |>

allEnumValues :: forall a. (Enum a, Bounded a) => [a]
allEnumValues = [minBound..maxBound]

join :: Maybe (Maybe a) -> Maybe a
join (Just (Just x)) = Just x
join _ = Nothing
