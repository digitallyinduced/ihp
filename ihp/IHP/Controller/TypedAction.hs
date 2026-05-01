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
Description: Typed controller actions with request body specs and colocated OpenAPI metadata

This module contains the opt-in typed action core. It is designed for GADT
action types where the action value carries path/query parameters and the action
indices carry the request body and response types.

The runtime action uses IHP-style helpers like 'bodyParam' and 'fillBody'
instead of taking a body argument directly:

@
data ProjectsAction request response where
    UpdateProjectAction
        :: { projectId :: Id Project, returnTo :: Maybe Text }
        -> ProjectsAction ('Body ProjectInput) EditView

instance Controller (ProjectsAction request response) where
    type ControllerAction (ProjectsAction request response) =
        ActionDef (ProjectsAction request response) request response

    action UpdateProjectAction { projectId, returnTo } =
        documented do
            summary "Update project"
        do
            let name = bodyParam #name
            ...
@

Path/query parameter documentation belongs to the typed route definition. The
action documentation block only describes operation metadata such as summary,
tags, success status, request body, and response.
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
    , bodyParam
    , fillBody
    , ActionDef (..)
    , documented
    , undocumented
    , TypedActionDoc (..)
    , TypedRequestBodyDoc (..)
    , TypedRouteDocument (..)
    , ParameterDoc (..)
    , ParameterLocation (..)
    , DocBuilder
    , summary
    , description
    , tags
    , operationId
    , successStatus
    , successResponseDescription
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
import IHP.Controller.Render (renderHtmlOrJsonWithStatusCode)
import IHP.ControllerSupport
import IHP.Record (CopyFields (..))
import IHP.RequestVault.ModelContext ()
import IHP.ViewSupport qualified as ViewSupport
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (StdMethod)
import Network.HTTP.Types.Status (Status, status200, status400, status415)
import Network.Wai (responseLBS)
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

-- | Read a field from the decoded typed request body.
bodyParam ::
    forall field input value.
    ( ?typedBody :: input
    , KnownSymbol field
    , HasField field input value
    ) =>
    Proxy field ->
    value
bodyParam _ = getField @field ?typedBody
{-# INLINE bodyParam #-}

-- | Copy a type-level list of fields from the decoded typed request body into
-- a destination record.
--
-- This only compiles when every listed field exists on both the decoded body
-- and the destination record with the same type.
fillBody ::
    forall fields model input.
    ( ?typedBody :: input
    , CopyFields fields model input
    ) =>
    model ->
    model
fillBody = copyFields @fields ?typedBody
{-# INLINE fillBody #-}

-- | Typed action definition. The runtime block can use '?typedBody', while the
-- documentation block is pure metadata and can be inspected without running
-- the action.
data ActionDef controller body response = ActionDef
    { runActionDef :: (?typedBody :: DecodedRequest body) => IO response
    , actionDefDoc :: Maybe (TypedActionDoc controller body response)
    , actionDefSuccessStatus :: !Status
    }

instance
    ( DecodeRequest body
    , ViewSupport.View response
    , ViewSupport.JsonView response
    , Typeable.Typeable response
    , JSON.ToJSON (ViewSupport.JsonResponse response)
    ) =>
    RunControllerAction controller (ActionDef controller body response)
    where
    runControllerActionDefault ActionDef{runActionDef, actionDefSuccessStatus} = do
        decodedBody <- decodeRequest @body
        case decodedBody of
            Left RequestDecodeError{requestDecodeErrorStatus, requestDecodeErrorMessage} ->
                respondAndExit (responseLBS requestDecodeErrorStatus [(hContentType, "text/plain")] (cs requestDecodeErrorMessage))
            Right typedBody -> do
                let ?typedBody = typedBody
                runActionDef >>= renderHtmlOrJsonWithStatusCode actionDefSuccessStatus
    {-# INLINE runControllerActionDefault #-}

-- | Creates a documented typed action. The response schema is inferred from
-- the runtime block's @IO response@ type.
documented ::
    forall controller body response.
    ( BuildTypedRequestBodyDoc body
    , ViewSupport.View response
    , ViewSupport.JsonView response
    , Typeable.Typeable response
    , JSON.ToJSON (ViewSupport.JsonResponse response)
    , ToSchema (ViewSupport.JsonResponse response)
    ) =>
    DocBuilder controller body response () ->
    ((?typedBody :: DecodedRequest body) => IO response) ->
    ActionDef controller body response
documented builder runAction =
    let draft = execDocBuilder builder defaultTypedActionDocDraft
        doc = buildTypedActionDoc @controller @body @response draft
     in ActionDef
            { runActionDef = runAction
            , actionDefDoc = Just doc
            , actionDefSuccessStatus = draft.draftSuccessStatus
            }
{-# INLINE documented #-}

-- | Creates a private typed action. The body still gets decoded and installed
-- as '?typedBody', but the route is omitted from typed OpenAPI metadata.
undocumented ::
    ((?typedBody :: DecodedRequest body) => IO response) ->
    ActionDef controller body response
undocumented runAction =
    ActionDef
        { runActionDef = runAction
        , actionDefDoc = Nothing
        , actionDefSuccessStatus = status200
        }
{-# INLINE undocumented #-}

-- | OpenAPI-style metadata for a documented typed action.
data TypedActionDoc controller body response where
    TypedActionDoc ::
        forall controller body response.
        ( ViewSupport.View response
        , ViewSupport.JsonView response
        , Typeable.Typeable response
        , JSON.ToJSON (ViewSupport.JsonResponse response)
        , ToSchema (ViewSupport.JsonResponse response)
        ) =>
        { typedActionDocSummary :: Maybe Text
        , typedActionDocDescription :: Maybe Text
        , typedActionDocTags :: [Text]
        , typedActionDocOperationId :: Maybe Text
        , typedActionDocRequestBody :: Maybe TypedRequestBodyDoc
        , typedActionDocResponse :: Proxy response
        , typedActionDocSuccessStatus :: Status
        , typedActionDocSuccessResponseDescription :: Text
        } ->
        TypedActionDoc controller body response

-- | Request body schema metadata for a documented typed action.
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

-- | A documented typed route with concrete routing metadata.
--
-- This is produced by typed route definitions and consumed by OpenAPI
-- generation. The action documentation itself still comes from the action's
-- colocated 'documented' block.
data TypedRouteDocument where
    TypedRouteDocument ::
        forall controller body response.
        { typedRouteDocumentName :: Text
        , typedRouteDocumentPath :: Text
        , typedRouteDocumentMethods :: [StdMethod]
        , typedRouteDocumentParameters :: [ParameterDoc]
        , typedRouteDocumentActionDoc :: TypedActionDoc controller body response
        } ->
        TypedRouteDocument

-- | Whether a documented parameter belongs in the route path or query string.
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

data TypedActionDocDraft = TypedActionDocDraft
    { draftSummary :: Maybe Text
    , draftDescription :: Maybe Text
    , draftTags :: [Text]
    , draftOperationId :: Maybe Text
    , draftSuccessStatus :: Status
    , draftSuccessResponseDescription :: Text
    }

defaultTypedActionDocDraft :: TypedActionDocDraft
defaultTypedActionDocDraft =
    TypedActionDocDraft
        { draftSummary = Nothing
        , draftDescription = Nothing
        , draftTags = []
        , draftOperationId = Nothing
        , draftSuccessStatus = status200
        , draftSuccessResponseDescription = "Successful response"
        }

-- | Metadata builder used by 'documented'.
newtype DocBuilder controller body response a = DocBuilder
    { runDocBuilder :: TypedActionDocDraft -> (a, TypedActionDocDraft)
    }

instance Functor (DocBuilder controller body response) where
    fmap f (DocBuilder run) =
        DocBuilder \draft ->
            let (value, draft') = run draft
             in (f value, draft')
    {-# INLINE fmap #-}

instance Applicative (DocBuilder controller body response) where
    pure value = DocBuilder \draft -> (value, draft)
    {-# INLINE pure #-}

    DocBuilder runFunction <*> DocBuilder runValue =
        DocBuilder \draft ->
            let (function, draft') = runFunction draft
                (value, draft'') = runValue draft'
             in (function value, draft'')
    {-# INLINE (<*>) #-}

instance Monad (DocBuilder controller body response) where
    DocBuilder run >>= next =
        DocBuilder \draft ->
            let (value, draft') = run draft
             in runDocBuilder (next value) draft'
    {-# INLINE (>>=) #-}

execDocBuilder :: DocBuilder controller body response () -> TypedActionDocDraft -> TypedActionDocDraft
execDocBuilder (DocBuilder run) draft = snd (run draft)
{-# INLINE execDocBuilder #-}

modifyDoc :: (TypedActionDocDraft -> TypedActionDocDraft) -> DocBuilder controller body response ()
modifyDoc update = DocBuilder \draft -> ((), update draft)
{-# INLINE modifyDoc #-}

-- | Sets the OpenAPI operation summary.
summary :: Text -> DocBuilder controller body response ()
summary value = modifyDoc \draft -> draft{draftSummary = Just value}
{-# INLINE summary #-}

-- | Sets the OpenAPI operation description.
description :: Text -> DocBuilder controller body response ()
description value = modifyDoc \draft -> draft{draftDescription = Just value}
{-# INLINE description #-}

-- | Sets the OpenAPI operation tags.
tags :: [Text] -> DocBuilder controller body response ()
tags value = modifyDoc \draft -> draft{draftTags = value}
{-# INLINE tags #-}

-- | Sets the OpenAPI operation id.
operationId :: Text -> DocBuilder controller body response ()
operationId value = modifyDoc \draft -> draft{draftOperationId = Just value}
{-# INLINE operationId #-}

-- | Sets the documented and rendered success status.
successStatus :: Status -> DocBuilder controller body response ()
successStatus value = modifyDoc \draft -> draft{draftSuccessStatus = value}
{-# INLINE successStatus #-}

-- | Sets the OpenAPI success response description.
successResponseDescription :: Text -> DocBuilder controller body response ()
successResponseDescription value = modifyDoc \draft -> draft{draftSuccessResponseDescription = value}
{-# INLINE successResponseDescription #-}

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

buildTypedActionDoc ::
    forall controller body response.
    ( BuildTypedRequestBodyDoc body
    , ViewSupport.View response
    , ViewSupport.JsonView response
    , Typeable.Typeable response
    , JSON.ToJSON (ViewSupport.JsonResponse response)
    , ToSchema (ViewSupport.JsonResponse response)
    ) =>
    TypedActionDocDraft ->
    TypedActionDoc controller body response
buildTypedActionDoc draft =
    TypedActionDoc
        { typedActionDocSummary = draft.draftSummary
        , typedActionDocDescription = draft.draftDescription
        , typedActionDocTags = draft.draftTags
        , typedActionDocOperationId = draft.draftOperationId
        , typedActionDocRequestBody = typedRequestBodyDoc @body
        , typedActionDocResponse = Proxy @response
        , typedActionDocSuccessStatus = draft.draftSuccessStatus
        , typedActionDocSuccessResponseDescription = draft.draftSuccessResponseDescription
        }
{-# INLINE buildTypedActionDoc #-}

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
