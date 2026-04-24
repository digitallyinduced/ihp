{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module IHP.Controller.ActionDefinition
    ( ActionDefinition (..)
    , EndpointBuilder
    , endpoint
    , requestBodyIsRequired
    , responseView
    , summary
    , description
    , tags
    , operationId
    , successStatus
    , successResponseDescription
    , HandleEndpoint (handle)
    , legacyAction
    , actionDefinitionDoc
    ) where

import Data.Aeson qualified as JSON
import Data.ByteString (ByteString)
import Data.Data
import Data.Foldable (asum)
import Data.Int
import Data.Maybe (fromMaybe, mapMaybe)
import Data.OpenApi (ToSchema)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Data.Typeable qualified as Typeable
import Data.UUID qualified as UUID
import Data.Word
import IHP.Controller.Render (renderHtmlOrJsonWithStatusCode)
import IHP.ControllerSupport
import IHP.OpenApiSupport.ActionDoc
import IHP.ViewSupport qualified as ViewSupport
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Status (Status, status200, status400)
import Network.Wai (responseLBS)
import Prelude

-- | Inspectable controller action used by the endpoint DSL.
--
-- The documentation part is pure and can be read while generating OpenAPI.
-- The runner part is executed with the normal controller implicit parameters.
data ActionDefinition controller = ActionDefinition
    { actionDefinitionDocFor :: controller -> Maybe (ActionDoc controller)
    , runActionDefinition :: ControllerAction' controller
    }

instance RunControllerAction controller (ActionDefinition controller) where
    runControllerActionDefault = runActionDefinition
    {-# INLINABLE runControllerActionDefault #-}

instance
    ( Data controller
    , Controller controller
    , ControllerAction controller ~ ActionDefinition controller
    ) =>
    HasOpenApiActionDocs controller (ActionDefinition controller)
    where
    openApiActionDocs = actionDefinitionDocs @controller

-- | Builder used by 'endpoint', 'responseView' and metadata setters.
data EndpointBuilder controller view = EndpointBuilder
    { endpointDocModifiers :: [ActionDoc controller -> ActionDoc controller]
    , endpointSuccessStatus :: Status
    }

-- | Starts an inspectable endpoint definition.
endpoint :: EndpointBuilder controller view
endpoint =
    EndpointBuilder
        { endpointDocModifiers = []
        , endpointSuccessStatus = status200
        }
{-# INLINE endpoint #-}

addEndpointDocModifier ::
    (ActionDoc controller -> ActionDoc controller) ->
    EndpointBuilder controller view ->
    EndpointBuilder controller view
addEndpointDocModifier modifier endpointBuilder@EndpointBuilder{endpointDocModifiers} =
    endpointBuilder{endpointDocModifiers = endpointDocModifiers <> [modifier]}
{-# INLINE addEndpointDocModifier #-}

-- | Overrides whether the OpenAPI request body is required.
requestBodyIsRequired :: Bool -> EndpointBuilder controller view -> EndpointBuilder controller view
requestBodyIsRequired required =
    addEndpointDocModifier (setOpenApiRequestBodyRequired required)
{-# INLINE requestBodyIsRequired #-}

-- | Declares the view rendered by the endpoint.
responseView :: forall view controller previousView. EndpointBuilder controller previousView -> EndpointBuilder controller view
responseView EndpointBuilder{endpointDocModifiers, endpointSuccessStatus} =
    EndpointBuilder{endpointDocModifiers, endpointSuccessStatus}
{-# INLINE responseView #-}

-- | Sets the OpenAPI operation summary.
summary :: Text -> EndpointBuilder controller view -> EndpointBuilder controller view
summary text =
    addEndpointDocModifier (setOpenApiSummary text)
{-# INLINE summary #-}

-- | Sets the OpenAPI operation description.
description :: Text -> EndpointBuilder controller view -> EndpointBuilder controller view
description text =
    addEndpointDocModifier (setOpenApiDescription text)
{-# INLINE description #-}

-- | Sets the OpenAPI operation tags.
tags :: [Text] -> EndpointBuilder controller view -> EndpointBuilder controller view
tags values =
    addEndpointDocModifier (setOpenApiTags values)
{-# INLINE tags #-}

-- | Sets the OpenAPI operation id.
operationId :: Text -> EndpointBuilder controller view -> EndpointBuilder controller view
operationId text =
    addEndpointDocModifier (setOpenApiOperationId text)
{-# INLINE operationId #-}

-- | Sets the documented and rendered success status.
successStatus :: Status -> EndpointBuilder controller view -> EndpointBuilder controller view
successStatus status endpointBuilder =
    addEndpointDocModifier (setOpenApiSuccessStatus status) endpointBuilder{endpointSuccessStatus = status}
{-# INLINE successStatus #-}

-- | Sets the OpenAPI success response description.
successResponseDescription :: Text -> EndpointBuilder controller view -> EndpointBuilder controller view
successResponseDescription text =
    addEndpointDocModifier (setOpenApiSuccessResponseDescription text)
{-# INLINE successResponseDescription #-}

buildActionDoc ::
    forall view controller.
    ( Data controller
    , ViewSupport.View view
    , ViewSupport.JsonView view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    [ActionDoc controller -> ActionDoc controller] ->
    EndpointBuilder controller view ->
    controller ->
    ActionDoc controller
buildActionDoc initialDocModifiers EndpointBuilder{endpointDocModifiers} controller =
    foldl'
        (\doc modifier -> modifier doc)
        (actionDoc @view (cs (showConstr (toConstr controller))))
        (initialDocModifiers <> endpointDocModifiers)
{-# INLINE buildActionDoc #-}

-- | Finalizes an endpoint definition and records OpenAPI metadata from the handler type.
class HandleEndpoint controller handler view | handler -> view where
    -- | Finalizes an endpoint definition with its handler.
    --
    -- An @IO view@ handler has no request body. A @body -> IO view@ handler
    -- decodes that JSON body and uses the same body type for OpenAPI.
    handle :: handler -> EndpointBuilder controller view -> ActionDefinition controller

instance
    ( Data controller
    , ViewSupport.View view
    , ViewSupport.JsonView view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    HandleEndpoint controller (IO view) view
    where
    handle viewAction builder@EndpointBuilder{endpointSuccessStatus} =
        ActionDefinition
            { actionDefinitionDocFor = Just . buildActionDoc @view [] builder
            , runActionDefinition = viewAction >>= renderHtmlOrJsonWithStatusCode endpointSuccessStatus
            }

instance
    {-# OVERLAPPABLE #-}
    ( Data controller
    , JSON.FromJSON body
    , ToSchema body
    , Typeable.Typeable body
    , ViewSupport.View view
    , ViewSupport.JsonView view
    , Typeable.Typeable view
    , JSON.ToJSON (ViewSupport.JsonResponse view)
    , ToSchema (ViewSupport.JsonResponse view)
    ) =>
    HandleEndpoint controller (body -> IO view) view
    where
    handle viewAction builder@EndpointBuilder{endpointSuccessStatus} =
        ActionDefinition
            { actionDefinitionDocFor = Just . buildActionDoc @view [setOpenApiRequestBody @body] builder
            , runActionDefinition = do
                rawBody <- getRequestBody
                case JSON.eitherDecode rawBody of
                    Left errorMessage ->
                        respondAndExit
                            ( responseLBS
                                status400
                                [(hContentType, "text/plain")]
                                (cs ("Invalid JSON request body: " <> errorMessage))
                            )
                    Right body ->
                        viewAction body >>= renderHtmlOrJsonWithStatusCode endpointSuccessStatus
            }

-- | Wraps a classic controller action in an inspectable controller.
--
-- The wrapped action is executed normally and omitted from OpenAPI docs.
legacyAction :: ControllerAction' controller -> ActionDefinition controller
legacyAction controllerAction =
    ActionDefinition
        { actionDefinitionDocFor = const Nothing
        , runActionDefinition = controllerAction
        }
{-# INLINE legacyAction #-}

-- | Reads the OpenAPI documentation for a concrete action value.
actionDefinitionDoc :: controller -> ActionDefinition controller -> Maybe (ActionDoc controller)
actionDefinitionDoc controller ActionDefinition{actionDefinitionDocFor} =
    actionDefinitionDocFor controller
{-# INLINE actionDefinitionDoc #-}

-- | Builds OpenAPI docs from the same inspectable action definitions used at runtime.
actionDefinitionDocs ::
    forall controller.
    ( Controller controller
    , Data controller
    , ControllerAction controller ~ ActionDefinition controller
    ) =>
    [ActionDoc controller]
actionDefinitionDocs =
    dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
        |> mapMaybe (buildDoc . fromConstrB dummyDataValue)
  where
    buildDoc controller =
        let ?context = error "actionDefinitionDocs: endpoint construction must not use ?context"
            ?modelContext = error "actionDefinitionDocs: endpoint construction must not use ?modelContext"
            ?theAction = controller
            ?respond = error "actionDefinitionDocs: endpoint construction must not use ?respond"
            ?request = error "actionDefinitionDocs: endpoint construction must not use ?request"
         in actionDefinitionDoc controller (action controller)
{-# INLINE actionDefinitionDocs #-}

dummyDataValue :: forall value. (Data value) => value
dummyDataValue =
    fromMaybe fromFirstConstructor knownDummyValue
  where
    knownDummyValue =
        asum
            [ Typeable.cast (0 :: Int)
            , Typeable.cast (0 :: Int8)
            , Typeable.cast (0 :: Int16)
            , Typeable.cast (0 :: Int32)
            , Typeable.cast (0 :: Int64)
            , Typeable.cast (0 :: Integer)
            , Typeable.cast (0 :: Word)
            , Typeable.cast (0 :: Word8)
            , Typeable.cast (0 :: Word16)
            , Typeable.cast (0 :: Word32)
            , Typeable.cast (0 :: Word64)
            , Typeable.cast ("" :: Text)
            , Typeable.cast ("" :: ByteString)
            , Typeable.cast UUID.nil
            ]

    fromFirstConstructor =
        case dataTypeConstrs (dataTypeOf (Prelude.undefined :: value)) of
            constructor : _ -> fromConstrB dummyDataValue constructor
            [] -> error ("actionDefinitionDocs: cannot build dummy value for " <> show (Typeable.typeRep (Proxy @value)))
{-# INLINE dummyDataValue #-}
