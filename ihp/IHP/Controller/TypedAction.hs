{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module: IHP.Controller.TypedAction
Description: Typed controller actions with request body specs

This module contains the opt-in typed action core. It is designed for GADT
action types where the action value carries path/query parameters and the action
indices carry the request body and response types.

Actions with a request body take the decoded body as their second argument:

@
data ProjectsAction request response where
    IndexProjectsAction :: ProjectsAction 'NoBody IndexView
    UpdateProjectAction :: { projectId :: Id Project, returnTo :: Maybe Text } -> ProjectsAction ('Body ProjectInput) EditView

instance TypedController ProjectsAction where
    action IndexProjectsAction () = do
        ...
    action UpdateProjectAction { projectId, returnTo } body = do
        let name = bodyParam body #name
            ...
@

The route DSL owns the HTTP contract. Actions only contain runtime behavior.
-}
module IHP.Controller.TypedAction
    ( BodyEncoding (..)
    , BodySpec (..)
    , DefaultBodyEncodings
    , DecodedRequest
    , BodyEncodings
    , KnownBodyEncoding (..)
    , KnownBodyEncodings (..)
    , HasBodyEncoding
    , encodingContentType
    , encodingMediaType
    , RequestDecodeError (..)
    , FromJsonBody (..)
    , FromFormBody (..)
    , formBodyParam
    , genericParseFormBody
    , FromMultipartBody (..)
    , DecodeRequest (..)
    , TypedController (..)
    , RenderTypedResponse
    , BuildTypedRequestBodyDoc (..)
    , bodyParam
    , fillBody
    , TypedRequestBodyDoc (..)
    , TypedRouteDocument (..)
    , ParameterDoc (..)
    , ParameterLocation (..)
    , mkTypedRouteDocument
    ) where

import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as ByteString
import Data.Char (toLower)
import Data.Kind (Type)
import Data.OpenApi (ToSchema)
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Typeable qualified as Typeable
import GHC.Generics
import GHC.Records (HasField (..))
import GHC.TypeLits
import IHP.Controller.Param (ParamException (..), ParamReader)
import IHP.ControllerSupport (ControllerContext, Request, Respond, getRequestBody)
import IHP.ModelSupport (ModelContext)
import IHP.Record (CopyFields (..))
import IHP.RequestVault.ModelContext ()
import IHP.ViewSupport qualified as ViewSupport
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.Status (Status, status400, status415)
import Network.Wai (requestHeaders)
import Prelude
import Wai.Request.Params qualified as Params

-- | A high-level request body declaration.
--
-- Path and query params are constructor fields on the action value. They are
-- intentionally not part of 'BodySpec'.
data BodySpec
    = NoBody
    | Body Type
    | BodyWith Type [BodyEncoding]

-- | Default body encodings for 'Body': browser forms and JSON API clients.
type DefaultBodyEncodings = '[ 'FormUrlEncoded, 'Json]

-- | Runtime Haskell value produced after decoding a 'BodySpec'.
type family DecodedRequest (request :: BodySpec) :: Type where
    DecodedRequest 'NoBody = ()
    DecodedRequest ('Body input) = input
    DecodedRequest ('BodyWith input encodings) = input

-- | Accepted encodings for a 'BodySpec'.
type family BodyEncodings (request :: BodySpec) :: [BodyEncoding] where
    BodyEncodings 'NoBody = '[]
    BodyEncodings ('Body input) = DefaultBodyEncodings
    BodyEncodings ('BodyWith input encodings) = encodings

-- | Wire formats accepted for a typed request body.
data BodyEncoding
    = FormUrlEncoded
    | Json
    | Multipart
    deriving (Eq, Show)

-- | Runtime reflection for a type-level encoding.
class KnownBodyEncoding (encoding :: BodyEncoding) where
    bodyEncoding :: BodyEncoding

instance KnownBodyEncoding 'FormUrlEncoded where
    bodyEncoding = FormUrlEncoded
    {-# INLINE bodyEncoding #-}

instance KnownBodyEncoding 'Json where
    bodyEncoding = Json
    {-# INLINE bodyEncoding #-}

instance KnownBodyEncoding 'Multipart where
    bodyEncoding = Multipart
    {-# INLINE bodyEncoding #-}

-- | Runtime reflection for a type-level list of encodings.
class KnownBodyEncodings (encodings :: [BodyEncoding]) where
    bodyEncodings :: [BodyEncoding]

instance KnownBodyEncodings '[] where
    bodyEncodings = []
    {-# INLINE bodyEncodings #-}

instance (KnownBodyEncoding encoding, KnownBodyEncodings rest) => KnownBodyEncodings (encoding ': rest) where
    bodyEncodings = bodyEncoding @encoding : bodyEncodings @rest
    {-# INLINE bodyEncodings #-}

-- | Compile-time evidence that an encoding is accepted by a type-level
-- encoding list.
class HasBodyEncoding (encoding :: BodyEncoding) (encodings :: [BodyEncoding])

instance {-# OVERLAPPING #-} HasBodyEncoding encoding (encoding ': rest)

instance {-# OVERLAPPABLE #-} (HasBodyEncoding encoding rest) => HasBodyEncoding encoding (other ': rest)

-- | HTTP media type for a 'BodyEncoding'.
encodingContentType :: BodyEncoding -> ByteString
encodingContentType = \case
    FormUrlEncoded -> "application/x-www-form-urlencoded"
    Json -> "application/json"
    Multipart -> "multipart/form-data"
{-# INLINE encodingContentType #-}

-- | Text version of 'encodingContentType', useful for OpenAPI generation.
encodingMediaType :: BodyEncoding -> Text
encodingMediaType = cs . encodingContentType
{-# INLINE encodingMediaType #-}

-- | Request body decoding error. The status is normally 400 for invalid input
-- and 415 for unsupported content types.
data RequestDecodeError = RequestDecodeError
    { requestDecodeErrorStatus :: !Status
    , requestDecodeErrorMessage :: !Text
    }
    deriving (Eq, Show)

-- | Decodes JSON request bodies into typed input values.
class FromJsonBody input where
    parseJsonBody :: ByteString -> Either RequestDecodeError input

instance {-# OVERLAPPABLE #-} (JSON.FromJSON input) => FromJsonBody input where
    parseJsonBody payload =
        case JSON.eitherDecodeStrict payload of
            Right value -> Right value
            Left errorMessage ->
                Left
                    RequestDecodeError
                        { requestDecodeErrorStatus = status400
                        , requestDecodeErrorMessage = "Invalid JSON request body: " <> cs errorMessage
                        }
    {-# INLINE parseJsonBody #-}

-- | Decodes URL-encoded form bodies into typed input values.
class FromFormBody input where
    parseFormBody :: Request -> Either RequestDecodeError input

instance {-# OVERLAPPABLE #-} (Generic input, GFromFormBody (Rep input)) => FromFormBody input where
    parseFormBody = genericParseFormBody
    {-# INLINE parseFormBody #-}

-- | Read one URL-encoded form field from a request using IHP's existing
-- 'ParamReader' instances.
formBodyParam ::
    forall value.
    (ParamReader value) =>
    ByteString ->
    Request ->
    Either RequestDecodeError value
formBodyParam name request =
    case Params.paramOrError request.parsedBody request name of
        Right value -> Right value
        Left exception -> Left (paramExceptionToDecodeError exception)
{-# INLINE formBodyParam #-}

-- | Generic implementation for record-style request bodies.
--
-- This is used by the default 'FromFormBody' instance. You can also call it
-- from custom instances when you want to combine generic decoding with
-- application-specific parsing.
genericParseFormBody :: (Generic input, GFromFormBody (Rep input)) => Request -> Either RequestDecodeError input
genericParseFormBody request = to <$> gParseFormBody request
{-# INLINE genericParseFormBody #-}

-- | Decodes multipart request bodies into typed input values.
--
-- There is no generic default because file upload fields usually need
-- application-specific handling.
class FromMultipartBody input where
    parseMultipartBody :: Request -> Either RequestDecodeError input

-- | Decodes the current request according to a type-level 'BodySpec'.
class DecodeRequest (request :: BodySpec) where
    decodeRequest :: (?request :: Request) => IO (Either RequestDecodeError (DecodedRequest request))

instance DecodeRequest 'NoBody where
    decodeRequest = pure (Right ())
    {-# INLINE decodeRequest #-}

instance (DecodeBodyEncodings DefaultBodyEncodings input) => DecodeRequest ('Body input) where
    decodeRequest = decodeBodyWithAcceptedEncodings @DefaultBodyEncodings @input
    {-# INLINE decodeRequest #-}

instance (DecodeBodyEncodings encodings input) => DecodeRequest ('BodyWith input encodings) where
    decodeRequest = decodeBodyWithAcceptedEncodings @encodings @input
    {-# INLINE decodeRequest #-}

class DecodeBodyEncodings (encodings :: [BodyEncoding]) input where
    decodeBodyWithAcceptedEncodings :: (?request :: Request) => IO (Either RequestDecodeError input)

instance DecodeBodyEncodings '[] input where
    decodeBodyWithAcceptedEncodings = pure (Left unsupportedContentTypeError)
    {-# INLINE decodeBodyWithAcceptedEncodings #-}

instance
    ( KnownBodyEncoding encoding
    , DecodeBodyEncoding encoding input
    , DecodeBodyEncodings rest input
    ) =>
    DecodeBodyEncodings (encoding ': rest) input
    where
    decodeBodyWithAcceptedEncodings =
        case requestContentType ?request of
            Just contentType | contentTypeMatches (bodyEncoding @encoding) contentType -> decodeBodyEncoding @encoding @input
            _ -> decodeBodyWithAcceptedEncodings @rest @input
    {-# INLINE decodeBodyWithAcceptedEncodings #-}

class DecodeBodyEncoding (encoding :: BodyEncoding) input where
    decodeBodyEncoding :: (?request :: Request) => IO (Either RequestDecodeError input)

instance (FromJsonBody input) => DecodeBodyEncoding 'Json input where
    decodeBodyEncoding = pure . parseJsonBody @input . cs =<< getRequestBody
    {-# INLINE decodeBodyEncoding #-}

instance (FromFormBody input) => DecodeBodyEncoding 'FormUrlEncoded input where
    decodeBodyEncoding = pure (parseFormBody @input ?request)
    {-# INLINE decodeBodyEncoding #-}

instance (FromMultipartBody input) => DecodeBodyEncoding 'Multipart input where
    decodeBodyEncoding = pure (parseMultipartBody @input ?request)
    {-# INLINE decodeBodyEncoding #-}

requestContentType :: Request -> Maybe ByteString
requestContentType request =
    fmap normalizeContentType (lookup hContentType (requestHeaders request))
{-# INLINE requestContentType #-}

normalizeContentType :: ByteString -> ByteString
normalizeContentType =
    ByteString.map toLower . ByteString.takeWhile (/= ';')
{-# INLINE normalizeContentType #-}

contentTypeMatches :: BodyEncoding -> ByteString -> Bool
contentTypeMatches encoding contentType =
    let expected = encodingContentType encoding
     in case encoding of
            Json -> contentType == expected || "+json" `ByteString.isSuffixOf` contentType
            _ -> contentType == expected
{-# INLINE contentTypeMatches #-}

unsupportedContentTypeError :: (?request :: Request) => RequestDecodeError
unsupportedContentTypeError =
    RequestDecodeError
        { requestDecodeErrorStatus = status415
        , requestDecodeErrorMessage =
            "Unsupported request content type"
                <> maybe "" (\contentType -> ": " <> cs contentType) (requestContentType ?request)
        }
{-# INLINE unsupportedContentTypeError #-}

type RenderTypedResponse response =
    ( ViewSupport.View response
    , ViewSupport.JsonView response
    , Typeable.Typeable response
    , JSON.ToJSON (ViewSupport.JsonResponse response)
    )

-- | Controller class for typed GADT action families.
--
-- A single instance covers all constructors of the GADT family. Routes provide
-- the concrete body and response indices at dispatch time.
class TypedController (controller :: BodySpec -> Type -> Type) where
    beforeAction ::
        ( ?context :: ControllerContext
        , ?modelContext :: ModelContext
        , ?theAction :: controller body response
        , ?request :: Request
        , ?respond :: Respond
        ) =>
        IO ()
    beforeAction = pure ()
    {-# INLINE beforeAction #-}

    action ::
        ( ?context :: ControllerContext
        , ?modelContext :: ModelContext
        , ?theAction :: controller body response
        , ?request :: Request
        , ?respond :: Respond
        ) =>
        controller body response ->
        DecodedRequest body ->
        IO response

-- | Read a field from a decoded typed request body.
bodyParam ::
    forall field input value.
    ( KnownSymbol field
    , HasField field input value
    ) =>
    input ->
    Proxy field ->
    value
bodyParam input _ = getField @field input
{-# INLINE bodyParam #-}

-- | Copy a type-level list of fields from a decoded typed request body into a
-- destination record.
--
-- This only compiles when every listed field exists on both the decoded body
-- and the destination record with the same type.
fillBody ::
    forall fields model input.
    (CopyFields fields model input) =>
    input ->
    model ->
    model
fillBody input = copyFields @fields input
{-# INLINE fillBody #-}

-- | Request body schema metadata for a typed route.
data TypedRequestBodyDoc where
    TypedRequestBodyDoc ::
        forall input.
        ( ToSchema input
        , Typeable.Typeable input
        ) =>
        { typedRequestBodySchema :: Proxy input
        , typedRequestBodyTypeRep :: Typeable.TypeRep
        , typedRequestBodyEncodings :: [BodyEncoding]
        } ->
        TypedRequestBodyDoc

-- | A typed route with concrete OpenAPI metadata.
data TypedRouteDocument where
    TypedRouteDocument ::
        forall response.
        ( ViewSupport.View response
        , ViewSupport.JsonView response
        , Typeable.Typeable response
        , JSON.ToJSON (ViewSupport.JsonResponse response)
        , ToSchema (ViewSupport.JsonResponse response)
        ) =>
        { typedRouteDocumentName :: Text
        , typedRouteDocumentPath :: Text
        , typedRouteDocumentMethods :: [StdMethod]
        , typedRouteDocumentParameters :: [ParameterDoc]
        , typedRouteDocumentSummary :: Maybe Text
        , typedRouteDocumentDescription :: Maybe Text
        , typedRouteDocumentTags :: [Text]
        , typedRouteDocumentOperationId :: Maybe Text
        , typedRouteDocumentRequestBody :: Maybe TypedRequestBodyDoc
        , typedRouteDocumentResponse :: Proxy response
        , typedRouteDocumentSuccessStatus :: Status
        , typedRouteDocumentSuccessResponseDescription :: Text
        } ->
        TypedRouteDocument

-- | Whether a parameter belongs in the route path or query string.
data ParameterLocation
    = PathParameter
    | QueryParameter
    deriving (Eq, Show)

-- | OpenAPI-style parameter metadata.
data ParameterDoc where
    ParameterDoc ::
        forall value.
        ( ToSchema value
        , Typeable.Typeable value
        ) =>
        { parameterName :: Text
        , parameterLocation :: ParameterLocation
        , parameterRequired :: Bool
        , parameterSchema :: Proxy value
        , parameterTypeRep :: Typeable.TypeRep
        } ->
        ParameterDoc

class BuildTypedRequestBodyDoc (body :: BodySpec) where
    typedRequestBodyDoc :: Maybe TypedRequestBodyDoc

instance BuildTypedRequestBodyDoc 'NoBody where
    typedRequestBodyDoc = Nothing
    {-# INLINE typedRequestBodyDoc #-}

instance
    ( ToSchema input
    , Typeable.Typeable input
    , KnownBodyEncodings DefaultBodyEncodings
    ) =>
    BuildTypedRequestBodyDoc ('Body input)
    where
    typedRequestBodyDoc =
        Just (mkTypedRequestBodyDoc @input @DefaultBodyEncodings)
    {-# INLINE typedRequestBodyDoc #-}

instance
    ( ToSchema input
    , Typeable.Typeable input
    , KnownBodyEncodings encodings
    ) =>
    BuildTypedRequestBodyDoc ('BodyWith input encodings)
    where
    typedRequestBodyDoc =
        Just (mkTypedRequestBodyDoc @input @encodings)
    {-# INLINE typedRequestBodyDoc #-}

mkTypedRequestBodyDoc ::
    forall input encodings.
    ( ToSchema input
    , Typeable.Typeable input
    , KnownBodyEncodings encodings
    ) =>
    TypedRequestBodyDoc
mkTypedRequestBodyDoc =
    TypedRequestBodyDoc
        { typedRequestBodySchema = Proxy @input
        , typedRequestBodyTypeRep = Typeable.typeRep (Proxy @input)
        , typedRequestBodyEncodings = bodyEncodings @encodings
        }
{-# INLINE mkTypedRequestBodyDoc #-}

mkTypedRouteDocument ::
    forall body response.
    ( BuildTypedRequestBodyDoc body
    , ViewSupport.View response
    , ViewSupport.JsonView response
    , Typeable.Typeable response
    , JSON.ToJSON (ViewSupport.JsonResponse response)
    , ToSchema (ViewSupport.JsonResponse response)
    ) =>
    Text ->
    Text ->
    [StdMethod] ->
    [ParameterDoc] ->
    Maybe Text ->
    Maybe Text ->
    [Text] ->
    Maybe Text ->
    Status ->
    Text ->
    TypedRouteDocument
mkTypedRouteDocument routeName routePath routeMethods routeParameters summary description tags operationId successStatus successResponseDescription =
    TypedRouteDocument
        { typedRouteDocumentName = routeName
        , typedRouteDocumentPath = routePath
        , typedRouteDocumentMethods = routeMethods
        , typedRouteDocumentParameters = routeParameters
        , typedRouteDocumentSummary = summary
        , typedRouteDocumentDescription = description
        , typedRouteDocumentTags = tags
        , typedRouteDocumentOperationId = operationId
        , typedRouteDocumentRequestBody = typedRequestBodyDoc @body
        , typedRouteDocumentResponse = Proxy @response
        , typedRouteDocumentSuccessStatus = successStatus
        , typedRouteDocumentSuccessResponseDescription = successResponseDescription
        }
{-# INLINE mkTypedRouteDocument #-}

class GFromFormBody f where
    gParseFormBody :: Request -> Either RequestDecodeError (f p)

instance GFromFormBody U1 where
    gParseFormBody _ = Right U1
    {-# INLINE gParseFormBody #-}

instance (GFromFormBody f) => GFromFormBody (M1 D meta f) where
    gParseFormBody = fmap M1 . gParseFormBody
    {-# INLINE gParseFormBody #-}

instance (GFromFormBody f) => GFromFormBody (M1 C meta f) where
    gParseFormBody = fmap M1 . gParseFormBody
    {-# INLINE gParseFormBody #-}

instance (GFromFormBody left, GFromFormBody right) => GFromFormBody (left :*: right) where
    gParseFormBody request =
        (:*:)
            <$> gParseFormBody request
            <*> gParseFormBody request
    {-# INLINE gParseFormBody #-}

instance
    ( KnownSymbol fieldName
    , ParamReader fieldType
    ) =>
    GFromFormBody (M1 S ('MetaSel ('Just fieldName) sourceUnpackedness sourceStrictness sourceDecidedStrictness) (K1 R fieldType))
    where
    gParseFormBody request =
        let name = cs (symbolVal (Proxy @fieldName))
         in M1 . K1 <$> formBodyParam name request
    {-# INLINE gParseFormBody #-}

paramExceptionToDecodeError :: ParamException -> RequestDecodeError
paramExceptionToDecodeError = \case
    ParamNotFoundException{name} ->
        RequestDecodeError
            { requestDecodeErrorStatus = status400
            , requestDecodeErrorMessage = "Missing form field: " <> cs name
            }
    ParamCouldNotBeParsedException{name, parserError} ->
        RequestDecodeError
            { requestDecodeErrorStatus = status400
            , requestDecodeErrorMessage = "Invalid form field " <> cs name <> ": " <> cs parserError
            }
{-# INLINE paramExceptionToDecodeError #-}
