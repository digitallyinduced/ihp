{-|
Module: IHP.ControllerContext
Description: Typed key-value context container with minimal dependencies
Copyright: (c) digitally induced GmbH, 2020

This module provides a typed key-value container where the key is the type of the value.
It only depends on base and typerep-map, making it suitable for packages that need
context storage without pulling in the full IHP dependency tree.

The main IHP framework has heavy transitive dependencies (database, mail, logging, etc.)
through FrameworkConfig. By extracting ControllerContext into this minimal package,
other IHP packages like ihp-pagehead can have a much smaller dependency footprint.
-}
module IHP.ControllerContext
    ( ControllerContext(..)
    , newControllerContext
    , freeze
    , unfreeze
    , putContext
    , fromContext
    , maybeFromContext
    , fromFrozenContext
    , maybeFromFrozenContext
    ) where

import Prelude
import Data.IORef
import qualified Data.TMap as TypeMap
import qualified Data.Typeable as Typeable
import Data.Typeable (Typeable)

-- | A container storing useful data along the request lifecycle, such as the request, the current user, set current view layout, flash messages, ...
--
-- The controller context is usually accessed via the @?context@ variable. It's available inside the action and the view. Think of it as a key-value-map where the key is the type of the value.
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
-- The context is initially created before a action is going to be executed. Its life cycle looks like this:
--
-- - @newControllerContext@: The new controller context is created
-- - The 'IHP.ControllerSupport.runActionWithNewContext' fills in a few default values: The current @?application@ and also the Flash Messages to be rendered in the to-be-generated response.
-- - @initContext@: The initContext function of the @InitControllerContext WebApplication@ (inside your FrontController.hs) is called. There application-specific context can be provided. Usually this is the current user and the default layout.
-- - @beforeAction@: Here the context could also be modified. E.g. the layout could be overriden here for the whole controller.
-- - @action ..@: The action itself.
-- - Freezing: Before rendering the response, the container is frozen. Frozen means that all previously mutable fields become immutable.
-- - View Rendering: The frozen container is now used inside the view and layout to display information such as the current user or flash messages
data ControllerContext
    = ControllerContext { customFieldsRef :: IORef TypeMap.TMap }
    | FrozenControllerContext { customFields :: TypeMap.TMap }

-- | Creates a new empty controller context
newControllerContext :: IO ControllerContext
newControllerContext = do
    customFieldsRef <- newIORef TypeMap.empty
    pure ControllerContext { customFieldsRef }
{-# INLINABLE newControllerContext #-}

-- | After freezing a container you can access its values from pure non-IO code by using 'fromFrozenContext'
--
-- Calls to 'putContext' will throw an exception after it's frozen.
freeze :: ControllerContext -> IO ControllerContext
freeze ControllerContext { customFieldsRef } = FrozenControllerContext <$> readIORef customFieldsRef
freeze frozen = pure frozen
{-# INLINABLE freeze #-}

-- | Returns an unfrozen version of the controller context that can be modified again
--
-- This is used together with 'freeze' by e.g. AutoRefresh to make an immutable copy of the current controller context state
unfreeze :: ControllerContext -> IO ControllerContext
unfreeze FrozenControllerContext { customFields } = do
    customFieldsRef <- newIORef customFields
    pure ControllerContext { customFieldsRef }
unfreeze ControllerContext {} = error "Cannot call unfreeze on a controller context that is not frozen"
{-# INLINABLE unfreeze #-}

-- | Returns a value from the current controller context
--
-- Throws an exception if there is no value with the type inside the context
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
        let notFoundMessage = buildNotFoundMessage (Typeable.typeRep (Typeable.Proxy @value)) customFields
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
        let notFoundMessage = buildNotFoundMessage (Typeable.typeRep (Typeable.Proxy @value)) customFields
        error notFoundMessage
{-# INLINABLE fromFrozenContext #-}

-- | Returns a value from the current controller context, or Nothing if not found
maybeFromContext :: forall value. (?context :: ControllerContext, Typeable value) => IO (Maybe value)
maybeFromContext = do
    frozen <- freeze ?context
    let ?context = frozen
    pure (maybeFromFrozenContext @value)
{-# INLINABLE maybeFromContext #-}

-- | Returns a value from a frozen controller context, or Nothing if not found
maybeFromFrozenContext :: forall value. (?context :: ControllerContext, Typeable value) => Maybe value
maybeFromFrozenContext = case ?context of
    FrozenControllerContext { customFields } -> TypeMap.lookup @value customFields
    ControllerContext {} -> error ("maybeFromFrozenContext called on a non frozen context while trying to access " <> show (Typeable.typeRep (Typeable.Proxy @value)))
{-# INLINABLE maybeFromFrozenContext #-}

-- | Puts a value into the context
--
-- Throws an exception if the context is already frozen.
putContext :: forall value. (?context :: ControllerContext, Typeable value) => value -> IO ()
putContext value = do
    let ControllerContext { customFieldsRef } = ?context
    modifyIORef customFieldsRef (TypeMap.insert value)
{-# INLINABLE putContext #-}

-- | Build an improved error message when a value is not found in the context
--
-- For common types that require initialization, provides helpful suggestions
buildNotFoundMessage :: Typeable.TypeRep -> TypeMap.TMap -> String
buildNotFoundMessage typeRep customFields =
    let typeName = show typeRep
        baseMessage = "Unable to find " <> typeName <> " in controller context: " <> show customFields
        helpMessage = case findHint typeName knownTypes of
            Just hint -> "\n\nHint: " <> hint
            Nothing -> ""
    in baseMessage <> helpMessage
  where
    -- Map of type names to helpful hints for initialization
    knownTypes =
        [ ("AutoRefreshState", "Ensure you have called 'initAutoRefresh' in your 'initContext' function in FrontController.hs")
        , ("Maybe User", "Ensure you have called 'initAuthentication @User' in your 'initContext' function in FrontController.hs")
        , ("Maybe Admin", "Ensure you have called 'initAuthentication @Admin' in your 'initContext' function in FrontController.hs")
        , ("PageTitle", "Use 'setTitle' to set the page title (imported from IHP.PageHead.ControllerFunctions)")
        ]

    -- Helper function for finding a hint based on the type name
    -- Handles both qualified (IHP.AutoRefresh.Types.AutoRefreshState) and unqualified (AutoRefreshState) names
    findHint :: String -> [(String, String)] -> Maybe String
    findHint target list = 
        let matchesType key = target == key || endsWith key target
            endsWith suffix str = 
                let len = length suffix
                    strLen = length str
                in strLen >= len && drop (strLen - len) str == suffix && 
                   (strLen == len || str !! (strLen - len - 1) == '.')
        in foldr (\(key, value) acc ->
            if matchesType key then Just value else acc) Nothing list
{-# INLINABLE buildNotFoundMessage #-}
