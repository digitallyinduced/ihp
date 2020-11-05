{-|
Module: IHP.Controller.Context
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Context where

import IHP.Prelude
import qualified Data.TMap as TypeMap
import qualified Data.Typeable as Typeable
import IHP.Controller.RequestContext
import IHP.FrameworkConfig

data ControllerContext = ControllerContext { requestContext :: RequestContext, customFieldsRef :: IORef TypeMap.TMap }
                       | FrozenControllerContext { requestContext :: RequestContext, customFields :: TypeMap.TMap }

newControllerContext :: (?requestContext :: RequestContext) => IO ControllerContext
newControllerContext = do
    customFieldsRef <- newIORef TypeMap.empty
    pure ControllerContext { requestContext = ?requestContext, customFieldsRef}
{-# INLINE newControllerContext #-}

freeze :: ControllerContext -> IO ControllerContext
freeze ControllerContext { requestContext, customFieldsRef } = FrozenControllerContext requestContext <$> readIORef customFieldsRef
freeze frozen = pure frozen

fromContext :: forall value. (?context :: ControllerContext, Typeable value) => IO value
fromContext = maybeFromContext @value >>= \case
        Just value -> pure value
        Nothing -> do
            let ControllerContext { customFieldsRef } = ?context
            customFields <- readIORef customFieldsRef
            let notFoundMessage = ("Unable to find " <> (show (Typeable.typeRep (Typeable.Proxy @value))) <> " in controller context: " <> show customFields)
            
            error notFoundMessage
{-# INLINE fromContext #-}

fromFrozenContext :: forall value. (?context :: ControllerContext, Typeable value) => value
fromFrozenContext = case maybeFromFrozenContext @value of
        Just value -> value
        Nothing -> do
            let FrozenControllerContext { customFields } = ?context
            let notFoundMessage = ("Unable to find " <> (show (Typeable.typeRep (Typeable.Proxy @value))) <> " in controller context: " <> show customFields)
            
            error notFoundMessage
{-# INLINE fromFrozenContext #-}

maybeFromContext :: forall value. (?context :: ControllerContext, Typeable value) => IO (Maybe value)
maybeFromContext = do
    frozen <- freeze ?context
    let ?context = frozen
    pure (maybeFromFrozenContext @value)
{-# INLINE maybeFromContext #-}

maybeFromFrozenContext :: forall value. (?context :: ControllerContext, Typeable value) => Maybe value
maybeFromFrozenContext = case ?context of
        FrozenControllerContext { customFields } -> TypeMap.lookup @value customFields
        ControllerContext {} -> error "maybeFromFrozenContext called on a non frozen context"
{-# INLINE maybeFromFrozenContext #-}

putContext :: forall value. (?context :: ControllerContext, Typeable value) => value -> IO ()
putContext value = do
    let ControllerContext { customFieldsRef } = ?context
    modifyIORef customFieldsRef (TypeMap.insert value)
    pure ()

newtype ActionType = ActionType Typeable.TypeRep

instance ConfigProvider ControllerContext where
    getFrameworkConfig context = getFrameworkConfig (get #requestContext context)