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
import IHP.Log.Types

-- | A container storing useful data along the request lifecycle, such as the request, the current user, set current view layout, flash messages, ...
--
-- The controller context is usually accessed via the @?context@ variable. It's availble inside the action and the view. Think of it as a key-value-map where the key is the type of the value.
--
-- You can store information inside the context using 'putContext':
--
-- >>> newtype CurrentLayout = CurrentLayout Html
-- >>>
-- >>> ?context <- newControllerContext
-- >>> putContext (CurrentLayout layout)
--
-- Inside an action you can access the values using 'fromContext':
--
-- >>> (CurrentLayout layout) <- fromContext
--
-- You can freeze the context and then access values without being inside an IO context (like inside views which are pure):
--
-- Call 'freeze' inside an IO part:
--
-- >>> ?context <- freeze ?context
--
-- ('freeze' is automatically called by IHP before rendering a view, so usually you don't need to call it manually)
--
-- Then use the frozen context from your pure code like this:
--
-- >>> let (CurrentLayout layout) = fromFrozenContext in ...
--
-- The context is initially created before a action is going to be executed. It's life cycle looks like this:
--
-- - @newControllerContext@: The new controller context is created
-- - The 'IHP.ControllerSupport.runActionWithNewContext' fills in a few default values: The current @?application@ and also the Flash Messages to be rendered in the to-be-generated response.
-- - @initContext@: The initContext function of the @InitControllerContext WebApplication@ (inside your FrontController.hs) is called. There application-specific context can be provided. Usually this is the current user and the default layout.
-- - @beforeAction@: Here the context could also be modified. E.g. the layout could be overriden here for the whole controller.
-- - @action ..@: The action itself.
-- - Freezing: Before rendering the response, the container is frozen. Frozen means that all previously mutable fields become immutable.
-- - View Rendering: The frozen container is now used inside the view and layout to display information such as the current user or flash messages
data ControllerContext = ControllerContext { requestContext :: RequestContext, customFieldsRef :: IORef TypeMap.TMap }
                       | FrozenControllerContext { requestContext :: RequestContext, customFields :: TypeMap.TMap }

newControllerContext :: (?requestContext :: RequestContext) => IO ControllerContext
newControllerContext = do
    customFieldsRef <- newIORef TypeMap.empty
    pure ControllerContext { requestContext = ?requestContext, customFieldsRef }
{-# INLINABLE newControllerContext #-}

-- | After freezing a container you can access it's values from pure non-IO code by using 'fromFronzenContext'
--
-- Calls to 'putContext' will throw an exception after it's frozen.
freeze :: ControllerContext -> IO ControllerContext
freeze ControllerContext { requestContext, customFieldsRef } = FrozenControllerContext requestContext <$> readIORef customFieldsRef
freeze frozen = pure frozen
{-# INLINABLE freeze #-}

-- | Returns a unfrozen version of the controller context that can be modified again
--
-- This is used together with 'freeze' by e.g. AutoRefresh to make a immutable copy of the current controller context state
unfreeze :: ControllerContext -> IO ControllerContext
unfreeze FrozenControllerContext { requestContext, customFields } = do
    customFieldsRef <- newIORef customFields
    pure ControllerContext { .. }
unfreeze ControllerContext {} = error "Cannot call unfreeze on a controller context that is not frozen"
{-# INLINABLE unfreeze #-}


-- | Returns a value from the current controller context
--
-- Throws an exception if the there is no value with the type inside the context
--
-- __Example:__ Read the current user from the context
--
-- >>> user <- fromContext @User
fromContext :: forall value. (?context :: ControllerContext, Typeable value) => IO value
fromContext = maybeFromContext @value >>= \case
        Just value -> pure value
        Nothing -> do
            let ControllerContext { customFieldsRef } = ?context
            customFields <- readIORef customFieldsRef
            let notFoundMessage = ("Unable to find " <> (show (Typeable.typeRep (Typeable.Proxy @value))) <> " in controller context: " <> show customFields)

            error notFoundMessage
{-# INLINABLE fromContext #-}

-- | Returns a value from the current controller context. Requires the context to be frozen.
--
-- __Example:__ Read the current user from the context
--
-- >>> let user = fromFrozenContext @User
fromFrozenContext :: forall value. (?context :: ControllerContext, Typeable value) => value
fromFrozenContext = case maybeFromFrozenContext @value of
        Just value -> value
        Nothing -> do
            let FrozenControllerContext { customFields } = ?context
            let notFoundMessage = ("Unable to find " <> (show (Typeable.typeRep (Typeable.Proxy @value))) <> " in controller context: " <> show customFields)

            error notFoundMessage
{-# INLINABLE fromFrozenContext #-}

maybeFromContext :: forall value. (?context :: ControllerContext, Typeable value) => IO (Maybe value)
maybeFromContext = do
    frozen <- freeze ?context
    let ?context = frozen
    pure (maybeFromFrozenContext @value)
{-# INLINABLE maybeFromContext #-}

maybeFromFrozenContext :: forall value. (?context :: ControllerContext, Typeable value) => Maybe value
maybeFromFrozenContext = case ?context of
        FrozenControllerContext { customFields } -> TypeMap.lookup @value customFields
        ControllerContext {} -> error ("maybeFromFrozenContext called on a non frozen context while trying to access " <> (show (Typeable.typeRep (Typeable.Proxy @value))))
{-# INLINABLE maybeFromFrozenContext #-}

-- | Puts a value into the context
--
-- Throws an exception if the context is already frozen.
putContext :: forall value. (?context :: ControllerContext, Typeable value) => value -> IO ()
putContext value = do
    let ControllerContext { customFieldsRef } = ?context
    modifyIORef customFieldsRef (TypeMap.insert value)
    pure ()
{-# INLINABLE putContext #-}

newtype ActionType = ActionType Typeable.TypeRep

instance ConfigProvider ControllerContext where
    getFrameworkConfig context = getFrameworkConfig (get #requestContext context)
    {-# INLINABLE getFrameworkConfig #-}

instance LoggingProvider ControllerContext where
    getLogger = getLogger . getFrameworkConfig