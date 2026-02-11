{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, BlockArguments, DataKinds #-}

{-|
Module: IHP.Controller.Param
Description: Accessing query parameters and the request body
Copyright: (c) digitally induced GmbH, 2020

This module provides IHP-style parameter parsing using implicit parameters.
It wraps the generic functionality from "Wai.Request.Params" with IHP's
implicit @?request@ convention and adds IHP-specific 'ParamReader' instances.
-}
module IHP.Controller.Param
( -- * Reading parameters (implicit param versions)
  param
, paramOrNothing
, paramOrDefault
, paramOrError
, paramList
, paramListOrNothing
, hasParam
, queryOrBodyParam
, allParams
  -- * Specialized param functions
, paramText
, paramInt
, paramBool
, paramUUID
  -- * ParamReader typeclass (re-exported from Wai.Request.Params)
, ParamReader (..)
  -- * Exceptions (re-exported from Wai.Request.Params)
, ParamException (..)
  -- * Helper functions for custom ParamReader instances
, enumParamReader
, enumParamReaderJSON
  -- * Form filling
, FillParams (..)
, ifValid
, ifNew
  -- * Utilities
, emptyValueToNothing
) where

import IHP.Prelude
import Network.Wai (Request)
import qualified IHP.ModelSupport as ModelSupport
import IHP.ValidationSupport
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified Data.Aeson as Aeson
import IHP.RequestVault ()
import qualified Control.DeepSeq as DeepSeq
import Text.Read (readMaybe)

-- Import the generic implementation
import qualified Wai.Request.Params as Params
import Wai.Request.Params (ParamReader(..), ParamException(..), enumParamReaderJSON)

-- | Returns a query or body parameter from the current request. The raw string
-- value is parsed before returning it. So the return value type depends on what
-- you expect (e.g. can be Int, Text, UUID, Bool, some custom type).
--
-- When the parameter is missing or cannot be parsed, an exception is thrown and
-- the current action is aborted. Use 'paramOrDefault' when you want to get a
-- default value instead of an exception, or 'paramOrNothing' to get @Nothing@
-- when the parameter is missing.
--
-- You can define a custom parameter parser by defining a 'ParamReader' instance.
--
-- __Example:__ Accessing a query parameter.
--
-- Let's say the request is:
--
-- > GET /UsersAction?maxItems=50
--
-- We can read @maxItems@ like this:
--
-- > action UsersAction = do
-- >     let maxItems :: Int = param "maxItems"
--
--
-- __Example:__ Working with forms (Accessing a body parameter).
--
-- Let's say we have the following html form:
--
-- > <form method="POST" action="/HelloWorld"
-- >     <input type="text" name="firstname" placeholder="Your firstname" />
-- >     <button type="submit">Send</button>
-- > </form>
--
-- The form has firstname text field and a send button.
-- When the form is submitted, it's send to @/HelloWorld@.
--
-- The following action reads the value of the submitted firstname and prints out @Hello firstname@:
--
-- > action HelloWorldAction = do
-- >     let firstname = param "firstname"
-- >     renderPlain ("Hello " <> firstname)
--
--
-- __Example:__ Missing parameters
--
-- Let's say the request is:
--
-- > GET /HelloWorldAction
--
-- But the action requires us to provide a firstname, like:
--
-- > action HelloWorldAction = do
-- >     let firstname = param "firstname"
-- >     renderPlain ("Hello " <> firstname)
--
-- Running the request @GET /HelloWorldAction@ without the firstname parameter will cause an
-- 'ParamNotFoundException' to be thrown with:
--
-- > param: Parameter 'firstname' not found
param :: (?request :: Request) => (ParamReader valueType) => ByteString -> valueType
param !name = Params.param ?request.parsedBody ?request name
{-# INLINABLE param #-}

-- | Similiar to 'param' but works with multiple params. Useful when working with checkboxes.
--
-- Given a query like:
--
-- > ingredients=milk&ingredients=egg
--
-- This will return:
--
-- >>> paramList @Text "ingredients"
-- ["milk", "egg"]
--
-- When no parameter with the name is given, an empty list is returned:
--
-- >>> paramList @Text "not_given_in_url"
-- []
--
-- When a value cannot be parsed, this function will fail similiar to 'param'.
--
-- Related: https://stackoverflow.com/questions/63875081/how-can-i-pass-list-params-in-ihp-forms/63879113
paramList :: forall valueType. (?request :: Request, DeepSeq.NFData valueType, ParamReader valueType) => ByteString -> [valueType]
paramList name = Params.paramList ?request.parsedBody ?request name
{-# INLINABLE paramList #-}

-- | Similiar to 'paramOrNothing' but works with multiple params. This is useful when submitting multiple
-- input fields with the same name, and some may be empty.
--
-- Given a query like (note the `ingredients` in the middle that has no value):
--
-- > ingredients=milk&ingredients&ingredients=egg
--
-- This will return:
--
-- >>> paramListOrNothing @Text "ingredients"
-- [Just "milk", Nothing, Just "egg"]
--
-- When no parameter with the name is given, an empty list is returned:
--
-- >>> paramListOrNothing @Text "not_given_in_url"
-- []
--
--
paramListOrNothing :: forall valueType. (?request :: Request, DeepSeq.NFData valueType, ParamReader valueType) => ByteString -> [Maybe valueType]
paramListOrNothing name = Params.paramListOrNothing ?request.parsedBody ?request name
{-# INLINABLE paramListOrNothing #-}

-- | Specialized version of param for 'Text'.
--
-- This way you don't need to know about the type application syntax.
paramText :: (?request :: Request) => ByteString -> Text
paramText = param @Text

-- | Specialized version of param for 'Int'.
--
-- This way you don't need to know about the type application syntax.
paramInt :: (?request :: Request) => ByteString -> Int
paramInt = param @Int

-- | Specialized version of param for 'Bool'.
--
-- This way you don't need to know about the type application syntax.
paramBool :: (?request :: Request) => ByteString -> Bool
paramBool = param @Bool

-- | Specialized version of param for 'UUID'.
--
-- This way you don't need to know about the type application syntax.
paramUUID :: (?request :: Request) => ByteString -> UUID
paramUUID = param @UUID

-- | Returns @True@ when a parameter is given in the request via the query or request body.
--
-- Use 'paramOrDefault' when you want to use this for providing a default value.
--
-- __Example:__
--
-- Given the request @GET /HelloWorld@
--
-- > action HelloWorldAction = do
-- >     if hasParam "firstname"
-- >         then ...
-- >         else renderPlain "Please provide your firstname"
--
-- This will render @Please provide your firstname@ because @hasParam "firstname"@ returns @False@
hasParam :: (?request :: Request) => ByteString -> Bool
hasParam = Params.hasParam ?request.parsedBody ?request
{-# INLINABLE hasParam #-}

-- | Like 'param', but returns a default value when the parameter is missing instead of throwing
-- an exception.
--
-- Use 'paramOrNothing' when you want to get @Maybe@.
--
-- __Example:__ Pagination
--
-- When calling @GET /Users@ the variable @page@ will be set to the default value @0@.
--
-- > action UsersAction = do
-- >     let page :: Int = paramOrDefault 0 "page"
--
-- When calling @GET /Users?page=1@ the variable @page@ will be set to @1@.
paramOrDefault :: (?request :: Request) => ParamReader a => a -> ByteString -> a
paramOrDefault !defaultValue name = Params.paramOrDefault ?request.parsedBody ?request defaultValue name
{-# INLINABLE paramOrDefault #-}

-- | Like 'param', but returns @Nothing@ the parameter is missing instead of throwing
-- an exception.
--
-- Use 'paramOrDefault' when you want to deal with a default value.
--
-- __Example:__
--
-- When calling @GET /Users@ the variable @page@ will be set to @Nothing@.
--
-- > action UsersAction = do
-- >     let page :: Maybe Int = paramOrNothing "page"
--
-- When calling @GET /Users?page=1@ the variable @page@ will be set to @Just 1@.
paramOrNothing :: forall paramType. (?request :: Request) => ParamReader (Maybe paramType) => ByteString -> Maybe paramType
paramOrNothing !name = Params.paramOrNothing ?request.parsedBody ?request name
{-# INLINABLE paramOrNothing #-}

-- | Like 'param', but returns @Left "Some error message"@ if the parameter is missing or invalid
paramOrError :: forall paramType. (?request :: Request) => ParamReader paramType => ByteString -> Either ParamException paramType
paramOrError !name = Params.paramOrError ?request.parsedBody ?request name
{-# INLINABLE paramOrError #-}

-- | Returns a parameter without any parsing. Returns @Nothing@ when the parameter is missing.
queryOrBodyParam :: (?request :: Request) => ByteString -> Maybe ByteString
queryOrBodyParam !name = Params.queryOrBodyParam ?request.parsedBody ?request name
{-# INLINABLE queryOrBodyParam #-}

-- | Returns all params available in the current request
allParams :: (?request :: Request) => [(ByteString, Maybe ByteString)]
allParams = Params.allParams ?request.parsedBody ?request

-- IHP-specific ParamReader instances

instance ParamReader ModelSupport.Point where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (do x <- Attoparsec.double; Attoparsec.char ','; y <- Attoparsec.double; Attoparsec.endOfInput; pure (ModelSupport.fromCoordinates x y)) byteString of
            Right value -> Right value
            Left error -> Left "has to be two numbers with a comma, e.g. '1,2'"

    readParameterJSON (Aeson.String string) = let byteString :: ByteString = cs string in  readParameter byteString
    readParameterJSON _ = Left "Expected Point"

instance ParamReader ModelSupport.Interval where
    {-# INLINABLE readParameter #-}
    readParameter byteString = case readMaybe (cs byteString) of
        Just interval -> Right interval
        Nothing -> Left "Invalid interval"

    readParameterJSON (Aeson.String string) = case readMaybe (cs string) of
        Just interval -> Right interval
        Nothing -> Left "Invalid interval"
    readParameterJSON _ = Left "Expected String"

instance ParamReader ModelSupport.Inet where
    {-# INLINABLE readParameter #-}
    readParameter byteString = case readMaybe (cs byteString) of
        Just inet -> Right inet
        Nothing -> Left "Invalid IP address"

    readParameterJSON (Aeson.String string) = case readMaybe (cs string) of
        Just inet -> Right inet
        Nothing -> Left "Invalid IP address"
    readParameterJSON _ = Left "Expected String"



instance ParamReader ModelSupport.Polygon where
    {-# INLINABLE readParameter #-}
    readParameter byteString =
        let
            pointParser = do
                Attoparsec.char '('
                x <- Attoparsec.double
                Attoparsec.char ','
                y <- Attoparsec.double
                Attoparsec.char ')'
                pure (x, y)
            parser = do
                points <- pointParser `Attoparsec.sepBy` (Attoparsec.char ',')
                Attoparsec.endOfInput
                case ModelSupport.refineFromPointList points of
                    Just polygon -> pure polygon
                    Nothing -> fail "Polygon must have at least 3 points"
        in
        case Attoparsec.parseOnly parser byteString of
            Right value -> Right value
            Left error -> Left (cs error)

    readParameterJSON (Aeson.String string) = let byteString :: ByteString = cs string in readParameter byteString
    readParameterJSON _ = Left "Expected Polygon"

instance {-# OVERLAPS #-} (ParamReader (ModelSupport.PrimaryKey model')) => ParamReader (ModelSupport.Id' model') where
    {-# INLINABLE readParameter #-}
    readParameter uuid = ModelSupport.Id <$> readParameter uuid
    readParameterJSON value = ModelSupport.Id <$> readParameterJSON value

-- | Can be used as a default implementation for 'readParameter' for enum structures
--
-- __Example:__
--
-- > data Color = Yellow | Red | Blue deriving (Enum)
-- >
-- > instance ParamReader Color where
-- >     readParameter = enumParamReader
-- >     readParameterJSON = enumParamReaderJSON
enumParamReader :: forall parameter. (Enum parameter, ModelSupport.InputValue parameter) => ByteString -> Either ByteString parameter
enumParamReader string =
        case find (\value -> ModelSupport.inputValue value == string') allEnumValues of
            Just value -> Right value
            Nothing -> Left "Invalid value"
    where
        string' = cs string

-- | Provides the 'fill' function for mass-assignment of multiple parameters to a record
--
-- Accepts a type-level list of parameter names (type-list syntax is like @\@'["a", "b", "c"]@) and a record. Then each parameter is
-- read from the request using the 'param' API. The parameter value is written to the record
-- field.  Because the parameter is assigned to the record, the parameter name list can only
-- contain attribute names of the record.
--
-- When there is a parser error, the error will be attached as a validation error to the record. The
-- remaining parameters will continue to be read.
--
-- If a parameter is missing from the request, this will be ignored and the function proceeds as usual.
--
--
-- __Example:__
--
-- > action UpdateUserAction { userId } = do
-- >     user :: User <- fetch userId
-- >     user
-- >         |> fill @["firstname", "lastname", "email"]
--
-- This code will read the firstname, lastname and email from the request and assign them to the user.
class FillParams (params :: [Symbol]) record where
    fill :: (
        ?request :: Request
        , HasField "meta" record ModelSupport.MetaBag
        , SetField "meta" record ModelSupport.MetaBag
        ) => record -> record

instance FillParams ('[]) record where
    fill !record = record
    {-# INLINE fill #-}

instance (FillParams rest record
    , KnownSymbol fieldName
    , SetField fieldName record fieldType
    , ParamReader fieldType
    , HasField "meta" record ModelSupport.MetaBag
    , SetField "meta" record ModelSupport.MetaBag
    ) => FillParams (fieldName:rest) record where
    fill !record =
        let
            name :: ByteString = cs $! (symbolVal (Proxy @fieldName))
            record' = case paramOrError name of
                Right !(value :: fieldType) -> setField @fieldName value record
                Left ParamCouldNotBeParsedException { parserError } -> attachFailure (Proxy @fieldName) (cs parserError) record
                Left ParamNotFoundException {} -> record
        in
            fill @rest record'
    {-# INLINE fill #-}

ifValid :: (HasField "meta" model ModelSupport.MetaBag) => (Either model model -> IO r) -> model -> IO r
ifValid branch model = branch $! if ModelSupport.isValid model
    then Right model
    else Left model
{-# INLINE ifValid #-}

ifNew :: forall record. (?modelContext :: ModelSupport.ModelContext, HasField "meta" record MetaBag) => (record -> record) -> record -> record
ifNew thenBlock record = if ModelSupport.isNew record then thenBlock record else record


-- | Transforms @Just ""@ to @Nothing@
--
-- __Example:__ We have record called @Company@ with a optional field @comment :: Maybe Text@
--
-- When we have a form that submits the @comment@ field and the field is empty, it will not be @NULL@ inside the database,
-- instead it will be set to the empty string. To avoid this we can apply @emptyValueToNothing #comment@. This function
-- turns the empty string into a 'Nothing' value.
--
-- > action UpdateCompanyAction { companyId } = do
-- >     company <- fetch companyId
-- >     company
-- >         |> fill '["name", "comment"]
-- >         |> emptyValueToNothing #comment
-- >         |> updateRecord
emptyValueToNothing field = modify field (maybe Nothing (\value -> if null value then Nothing else Just value))
