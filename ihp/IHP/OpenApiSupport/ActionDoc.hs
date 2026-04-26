{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module IHP.OpenApiSupport.ActionDoc
    ( ActionDoc (..)
    , OpenApiRequestBodyDoc (..)
    , HasOpenApiActionDocs (..)
    , actionDoc
    , setOpenApiSummary
    , setOpenApiDescription
    , setOpenApiTags
    , setOpenApiOperationId
    , setOpenApiRequestBody
    , setOpenApiRequestBodyRequired
    , setOpenApiSuccessStatus
    , setOpenApiSuccessResponseDescription
    ) where

import Data.Aeson qualified as JSON
import Data.OpenApi (ToSchema)
import Data.OpenApi.Schema.Validation qualified as SchemaValidation
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Typeable qualified as Typeable
import IHP.ViewSupport qualified as ViewSupport
import Network.HTTP.Types.Status (Status, status200, statusCode)
import Prelude

data ActionDoc controller where
    ActionDoc ::
        forall controller view.
        ( ViewSupport.View view
        , Typeable.Typeable view
        , JSON.ToJSON (ViewSupport.JsonResponse view)
        , ToSchema (ViewSupport.JsonResponse view)
        ) =>
        { actionDocName :: Text
        , actionDocSummary :: Maybe Text
        , actionDocDescription :: Maybe Text
        , actionDocTags :: [Text]
        , actionDocOperationId :: Maybe Text
        , actionDocView :: Proxy view
        , actionDocTypedJson :: view -> JSON.Value
        , actionDocValidateJsonSchema :: view -> Maybe String
        , actionDocRequestBody :: Maybe OpenApiRequestBodyDoc
        , actionDocSuccessStatus :: Int
        , actionDocSuccessResponseDescription :: Text
        } ->
        ActionDoc controller

data OpenApiRequestBodyDoc where
    OpenApiRequestBodyDoc ::
        forall body.
        ( ToSchema body
        , Typeable.Typeable body
        ) =>
        { requestBodyRequired :: Bool
        , requestBodySchema :: Proxy body
        , requestBodyTypeRep :: Typeable.TypeRep
        } ->
        OpenApiRequestBodyDoc

class HasOpenApiActionDocs controller action where
    openApiActionDocs :: [ActionDoc controller]

actionDoc ::
    forall view controller.
    ( ViewSupport.View view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    Text -> ActionDoc controller
actionDoc actionName =
    ActionDoc
        { actionDocName = actionName
        , actionDocSummary = Nothing
        , actionDocDescription = Nothing
        , actionDocTags = []
        , actionDocOperationId = Nothing
        , actionDocView = Proxy @view
        , actionDocTypedJson = JSON.toJSON . ViewSupport.json
        , actionDocValidateJsonSchema = SchemaValidation.validatePrettyToJSON . ViewSupport.json
        , actionDocRequestBody = Nothing
        , actionDocSuccessStatus = statusCode status200
        , actionDocSuccessResponseDescription = "Successful response"
        }
{-# INLINE actionDoc #-}

setOpenApiSummary :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiSummary summary ActionDoc{actionDocName, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary = Just summary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiSummary #-}

setOpenApiDescription :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiDescription description ActionDoc{actionDocName, actionDocSummary, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription = Just description
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiDescription #-}

setOpenApiTags :: [Text] -> ActionDoc controller -> ActionDoc controller
setOpenApiTags tags ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags = tags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiTags #-}

setOpenApiOperationId :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiOperationId operationId ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId = Just operationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiOperationId #-}

setOpenApiRequestBody ::
    forall body controller.
    ( ToSchema body
    , Typeable.Typeable body
    ) =>
    ActionDoc controller ->
    ActionDoc controller
setOpenApiRequestBody ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody =
            Just
                OpenApiRequestBodyDoc
                    { requestBodyRequired = True
                    , requestBodySchema = Proxy @body
                    , requestBodyTypeRep = Typeable.typeRep (Proxy @body)
                    }
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiRequestBody #-}

setOpenApiRequestBodyRequired :: Bool -> ActionDoc controller -> ActionDoc controller
setOpenApiRequestBodyRequired required ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessStatus, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody = setRequired <$> actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription
        }
  where
    setRequired OpenApiRequestBodyDoc{requestBodySchema, requestBodyTypeRep} =
        OpenApiRequestBodyDoc
            { requestBodyRequired = required
            , requestBodySchema
            , requestBodyTypeRep
            }
{-# INLINE setOpenApiRequestBodyRequired #-}

setOpenApiSuccessStatus :: Status -> ActionDoc controller -> ActionDoc controller
setOpenApiSuccessStatus status ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessResponseDescription} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody
        , actionDocSuccessStatus = statusCode status
        , actionDocSuccessResponseDescription
        }
{-# INLINE setOpenApiSuccessStatus #-}

setOpenApiSuccessResponseDescription :: Text -> ActionDoc controller -> ActionDoc controller
setOpenApiSuccessResponseDescription description ActionDoc{actionDocName, actionDocSummary, actionDocDescription, actionDocTags, actionDocOperationId, actionDocView, actionDocTypedJson, actionDocValidateJsonSchema, actionDocRequestBody, actionDocSuccessStatus} =
    ActionDoc
        { actionDocName
        , actionDocSummary
        , actionDocDescription
        , actionDocTags
        , actionDocOperationId
        , actionDocView
        , actionDocTypedJson
        , actionDocValidateJsonSchema
        , actionDocRequestBody
        , actionDocSuccessStatus
        , actionDocSuccessResponseDescription = description
        }
{-# INLINE setOpenApiSuccessResponseDescription #-}
