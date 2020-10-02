{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts, AllowAmbiguousTypes, FlexibleInstances, IncoherentInstances, UndecidableInstances, PolyKinds, TypeInType, BlockArguments, DataKinds #-}

{-|
Module: IHP.Controller.Param
Description: Accessing query parameters and the request body
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Param where

import IHP.Prelude
import qualified Data.Either as Either
import qualified Data.Text.Read
import IHP.Controller.RequestContext
import qualified Network.Wai as Wai
import qualified Data.UUID as UUID
import qualified IHP.ModelSupport as ModelSupport
import qualified Data.ByteString.Char8 as Char8
import IHP.ValidationSupport
import GHC.TypeLits
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec
import qualified GHC.Float as Float
import qualified Control.Exception as Exception

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
param :: (?requestContext :: RequestContext) => (ParamReader valueType) => ByteString -> valueType
param !name = case paramOrNothing name of
    Just value -> Either.fromRight (error (paramParserErrorMessage name)) (readParameter value)
    Nothing -> Exception.throw (ParamNotFoundException name)
{-# INLINE param #-}

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
paramList :: forall valueType. (?requestContext :: RequestContext) => (ParamReader valueType) => ByteString -> [valueType]
paramList name =
    allParams
    |> filter (\(paramName, paramValue) -> paramName == name)
    |> mapMaybe (\(paramName, paramValue) -> paramValue)
    |> map (readParameter @valueType)
    |> map (Either.fromRight (error (paramParserErrorMessage name)))
{-# INLINE paramList #-}

paramParserErrorMessage name = "param: Parameter '" <> cs name <> "' is invalid"

-- | Thrown when a parameter is missing when calling 'param "myParam"' or related functions
data ParamNotFoundException = ParamNotFoundException ByteString deriving (Show)

instance Exception ParamNotFoundException where
    displayException (ParamNotFoundException name) = "param: Parameter '" <> cs name <> "' not found"

-- | Specialisied version of param for 'Text'.
--
-- This way you don't need to know about the type application syntax.
paramText :: (?requestContext :: RequestContext) => ByteString -> Text
paramText = param @Text

-- | Specialisied version of param for 'Int'.
--
-- This way you don't need to know about the type application syntax.
paramInt :: (?requestContext :: RequestContext) => ByteString -> Int
paramInt = param @Int

-- | Specialisied version of param for 'Bool'.
--
-- This way you don't need to know about the type application syntax.
paramBool :: (?requestContext :: RequestContext) => ByteString -> Bool
paramBool = param @Bool

-- | Specialisied version of param for 'UUID'.
--
-- This way you don't need to know about the type application syntax.
paramUUID :: (?requestContext :: RequestContext) => ByteString -> UUID
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
hasParam :: (?requestContext :: RequestContext) => ByteString -> Bool
hasParam = isJust . queryOrBodyParam
{-# INLINE hasParam #-}

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
paramOrDefault :: (?requestContext :: RequestContext) => ParamReader a => a -> ByteString -> a
paramOrDefault !defaultValue = fromMaybe defaultValue . paramOrNothing
{-# INLINE paramOrDefault #-}

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
paramOrNothing :: (?requestContext :: RequestContext) => ParamReader a => ByteString -> Maybe a
paramOrNothing !name = case queryOrBodyParam name of
    Just value -> case readParameter value of
        Left error -> Nothing
        Right value -> Just value
    Nothing -> Nothing
{-# INLINE paramOrNothing #-}

-- | Returns a parameter without any parsing. Returns @Nothing@ when the parameter is missing.
queryOrBodyParam :: (?requestContext :: RequestContext) => ByteString -> Maybe ByteString
queryOrBodyParam !name = join (lookup name allParams)
{-# INLINE queryOrBodyParam #-}

-- | Returns all params available in the current request
allParams :: (?requestContext :: RequestContext) => [(ByteString, Maybe ByteString)]
allParams = concat [(map (\(a, b) -> (a, Just b)) params), (Wai.queryString request)]
    where
        RequestContext { request, params } = ?requestContext

-- | Input parser for 'param'.
--
-- Parses the input bytestring. Returns @Left "some error"@ when there is an error parsing the value.
-- Returns @Right value@ when the parsing succeeded.
class ParamReader a where
    readParameter :: ByteString -> Either ByteString a

instance ParamReader ByteString where
    {-# INLINE readParameter #-}
    readParameter byteString = pure byteString

instance ParamReader Int where
    {-# INLINE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left error -> Left ("ParamReader Int: " <> cs error)

instance ParamReader Integer where
    {-# INLINE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.decimal <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left error -> Left ("ParamReader Int: " <> cs error)

instance ParamReader Double where
    {-# INLINE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.double <* Attoparsec.endOfInput) byteString of
            Right value -> Right value
            Left error -> Left ("ParamReader Dobule: " <> cs error)

instance ParamReader Float where
    {-# INLINE readParameter #-}
    readParameter byteString =
        case Attoparsec.parseOnly (Attoparsec.double <* Attoparsec.endOfInput) byteString of
            Right value -> Right (Float.double2Float value)
            Left error -> Left ("ParamReader Dobule: " <> cs error)

instance ParamReader Text where
    {-# INLINE readParameter #-}
    readParameter byteString = pure (cs byteString)

instance ParamReader value => ParamReader [value] where
    {-# INLINE readParameter #-}
    readParameter byteString =
        byteString
        |> Char8.split ','
        |> map readParameter
        |> Either.partitionEithers
        |> \case
            ([], values) -> Right values
            ((first:rest), _) -> Left first

-- | Parses a boolean.
--
-- Html form checkboxes usually use @on@ or @off@ for representation. These
-- values are supported here.
instance ParamReader Bool where
    {-# INLINE readParameter #-}
    readParameter on | on == cs (ModelSupport.inputValue True) = pure True
    readParameter _ = pure False

instance ParamReader UUID where
    {-# INLINE readParameter #-}
    readParameter byteString =
        case UUID.fromASCIIBytes byteString of
            Just uuid -> pure uuid
            Nothing -> Left "FromParamter UUID: Parse error"

instance ParamReader UTCTime where
    {-# INLINE readParameter #-}
    readParameter "" = Left "ParamReader UTCTime: Parameter missing"
    readParameter byteString =
        let
            input = (cs byteString)
            dateTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" input
            date = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" input
        in case dateTime of
            Nothing -> case date of
                Just value -> Right value
                Nothing -> Left "ParamReader UTCTime: Failed parsing"
            Just value -> Right value

instance ParamReader Day where
    {-# INLINE readParameter #-}
    readParameter "" = Left "ParamReader Day: Parameter missing"
    readParameter byteString =
        let
            input = (cs byteString)
            date = parseTimeM True defaultTimeLocale "%Y-%-m-%-d" input
        in case date of
            Just value -> Right value
            Nothing -> Left "ParamReader Day: Failed parsing"

instance {-# OVERLAPS #-} (ParamReader (ModelSupport.PrimaryKey model')) => ParamReader (ModelSupport.Id' model') where
    {-# INLINE readParameter #-}
    readParameter uuid =
        case (readParameter uuid) :: Either ByteString (ModelSupport.PrimaryKey model') of
            Right uuid -> pure (ModelSupport.Id uuid)
            Left error -> Left error

instance ParamReader param => ParamReader (Maybe param) where
    {-# INLINE readParameter #-}
    readParameter param =
        case (readParameter param) :: Either ByteString param of
            Right value -> Right (Just value)
            Left error | param == "" -> Right Nothing
            Left error -> Left error

-- | Custom error hint when the 'param' is called with do-notation
--
-- __Example:__
--
-- > action Example = do
-- >     myParam <- param "hello"
--
-- Now a custom type error will be shown telling the user to use @let myParam = param "hello"@ instead of do-notation.
instance (TypeError ('Text ("Use 'let x = param \"..\"' instead of 'x <- param \"..\"'" :: Symbol))) => ParamReader  (IO param) where
    readParameter _ = error "Unreachable"

-- | Can be used as a default implementation for 'readParameter' for enum structures
--
-- __Example:__
--
-- > data Color = Yellow | Red | Blue deriving (Enum)
-- >
-- > instance ParamReader Color
-- >     readParameter = enumParamReader
enumParamReader :: forall parameter. (Enum parameter, ModelSupport.InputValue parameter) => ByteString -> Either ByteString parameter
enumParamReader string =
        case find (\value -> ModelSupport.inputValue value == string') allValues of
            Just value -> Right value
            Nothing -> Left "Invalid value"
    where
        string' = cs string
        allValues = enumFrom (toEnum 0) :: [parameter]


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
-- This code will read the firstname, lastname and email from the request and aissgn them to the user.
class FillParams (params :: [Symbol]) record where
    fill :: (
        ?requestContext :: RequestContext
        , HasField "meta" record ModelSupport.MetaBag
        , SetField "meta" record ModelSupport.MetaBag
        ) => record -> record

instance FillParams ('[]) record where
    fill !record = record

instance (FillParams rest record
    , KnownSymbol fieldName
    , SetField fieldName record fieldType
    , ParamReader fieldType
    , HasField "meta" record ModelSupport.MetaBag
    , SetField "meta" record ModelSupport.MetaBag
    ) => FillParams (fieldName:rest) record where
    fill !record = do
        let name :: ByteString = cs $! (symbolVal (Proxy @fieldName))
        case paramOrNothing name of
            Just !paramValue ->
                case readParameter paramValue of
                    Left !error -> fill @rest (attachFailure (Proxy @fieldName) (cs error) record)
                    Right !(value :: fieldType) -> fill @rest (setField @fieldName value record)
            Nothing -> fill @rest record

ifValid :: (HasField "meta" model ModelSupport.MetaBag) => (Either model model -> IO r) -> model -> IO r
ifValid branch model = branch ((if null annotations then Right else Left) model)
    where
        annotations :: [(Text, Text)]
        annotations = getField @"annotations" meta
        meta :: ModelSupport.MetaBag
        meta = getField @"meta" model

ifNew :: forall record id. (?requestContext :: RequestContext, ?modelContext :: ModelSupport.ModelContext, HasField "id" record id, Default id, Eq id) => (record -> record) -> record -> record
ifNew thenBlock record = if ModelSupport.isNew record then thenBlock record else record


-- Transforms `Just ""` to `Nothing`
emptyValueToNothing field = modify field (maybe Nothing (\value -> if null value then Nothing else Just value))
