{-# LANGUAGE AllowAmbiguousTypes, UndecidableInstances, LambdaCase, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-|
Module: IHP.AutoRoute

The legacy AutoRoute typeclass for IHP. Was previously part of
'IHP.RouterSupport'; extracted into the optional @ihp-autoroute@ package
so apps that still depend on @instance AutoRoute X@ can opt in. New
apps should prefer the @[routes|...|]@ DSL from 'IHP.Router.DSL'.

To use this module, add @ihp-autoroute@ to your flake's
@haskellPackages@ and @import IHP.AutoRoute@ in your @Web.Routes@
module.
-}
module IHP.AutoRoute
    ( AutoRoute (..)
    , applyConstr
    , buildAutoRouteMap
    , createAction
    , updateAction
    , QueryParam (..)
    , parseUUIDOrTextId
    , parseRouteWithId
    ) where

import Prelude hiding (take)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List (isPrefixOf, find)
import Control.Monad (unless, join)
import Control.Applicative ((<|>), empty)
import qualified Control.Exception as Exception
import qualified IHP.ModelSupport as ModelSupport
import Data.UUID
import Network.HTTP.Types.Method
import Network.Wai
import IHP.RouterSupport
import IHP.ControllerSupport
import IHP.Router.Types (ControllerRoute (..), UnexpectedMethodException (..), TypedAutoRouteError (..))
import Data.Attoparsec.ByteString.Char8 (string, Parser, takeByteString)
import Network.HTTP.Types.URI
import GHC.TypeLits as T
import Data.Data
import qualified Control.Monad.State.Strict as State
import qualified Data.Text as Text
import qualified Data.List as List
import Unsafe.Coerce
import IHP.HaskellSupport hiding (get)
import qualified Data.ByteString.Char8 as ByteString
import Data.String.Conversions (cs)
import qualified Network.URI.Encode as URI
import qualified Data.Text.Encoding as Text
import Data.Dynamic
import qualified Data.HashMap.Strict as HashMap


-- | Each of these is tried when trying to parse an argument to a controller constructor (i.e. in IHP, an action).
-- The type @d@ is an the type of the argument, and all we know about this type that its conforms to @Data@.
--
-- The approach taken here is to make use of the type equality operator @:~:@
-- to check and see if @d@ happens to be a certain type. If it is,
-- by matching on Just Refl, we are able to use @d@ as the type we matched it to.
parseFuncs :: forall d idType. (Data d, Data idType) => (ByteString -> Maybe idType) -> [Maybe ByteString -> Either TypedAutoRouteError d]
parseFuncs parseIdType = [
            -- Try and parse @Int@ or @Maybe Int@
            \case
                Just queryValue -> case eqT :: Maybe (d :~: Int) of
                    Just Refl -> case ByteString.readInt queryValue of
                        Just (n, "") -> Right n
                        _ -> Left BadType { field = "", value = Just queryValue, expectedType = "Int" }
                    Nothing -> case eqT :: Maybe (d :~: Maybe Int) of
                        Just Refl -> Right $ case ByteString.readInt queryValue of
                            Just (n, "") -> Just n
                            _ -> Nothing
                        Nothing -> Left NotMatched
                Nothing -> case eqT :: Maybe (d :~: Maybe Int) of
                    Just Refl -> Right Nothing
                    Nothing -> Left NotMatched,

            \case
                Just queryValue -> case eqT :: Maybe (d :~: Integer) of
                    Just Refl -> case ByteString.readInteger queryValue of
                        Just (n, "") -> Right n
                        _ -> Left BadType { field = "", value = Just queryValue, expectedType = "Integer" }
                    Nothing -> case eqT :: Maybe (d :~: Maybe Integer) of
                        Just Refl -> Right $ case ByteString.readInteger queryValue of
                            Just (n, "") -> Just n
                            _ -> Nothing
                        Nothing -> Left NotMatched
                Nothing -> case eqT :: Maybe (d :~: Maybe Integer) of
                    Just Refl -> Right Nothing
                    Nothing -> Left NotMatched,

            -- Try and parse @Text@ or @Maybe Text@
            \case
                Just queryValue -> case eqT :: Maybe (d :~: Text) of
                    Just Refl -> Right $ cs queryValue
                    Nothing -> case eqT :: Maybe (d :~: Maybe Text) of
                        Just Refl -> Right $ Just $ cs queryValue
                        Nothing -> Left NotMatched
                Nothing -> case eqT :: Maybe (d :~: Maybe Text) of
                    Just Refl -> Right Nothing
                    Nothing -> Left NotMatched,
            \case
                Just queryValue -> case parseIdType queryValue of
                    Just idValue -> case eqT :: Maybe (d :~: idType) of
                        Just Refl -> Right idValue
                        Nothing -> Left NotMatched
                    Nothing -> Left NotMatched
                Nothing -> Left NotMatched,

            -- Try and parse @[Text]@. If value is not present then default to empty list.
            \queryValue -> case eqT :: Maybe (d :~: [Text]) of
                Just Refl -> case queryValue of
                    Just queryValue -> Right $ Text.splitOn "," (cs queryValue)
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            -- Try and parse @[Int]@. If value is not present then default to empty list.
            \queryValue -> case eqT :: Maybe (d :~: [Int]) of
                Just Refl -> case queryValue of
                    Just queryValue -> ByteString.split ',' queryValue
                        |> mapMaybe (\b -> case ByteString.readInt b of Just (n, "") -> Just n; _ -> Nothing)
                        |> Right
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            \queryValue -> case eqT :: Maybe (d :~: [Integer]) of
                Just Refl -> case queryValue of
                    Just queryValue -> ByteString.split ',' queryValue
                        |> mapMaybe (\b -> case ByteString.readInteger b of Just (n, "") -> Just n; _ -> Nothing)
                        |> Right
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            -- Try and parse a raw [UUID]
            \queryValue -> case eqT :: Maybe (d :~: [UUID]) of
                Just Refl -> case queryValue of
                    Just queryValue -> ByteString.split ',' queryValue
                        |> mapMaybe fromASCIIBytes
                        |> Right
                    Nothing -> Right []
                Nothing -> Left NotMatched,

            -- Try and parse a raw UUID
            \queryValue -> case eqT :: Maybe (d :~: UUID) of
                Just Refl -> case queryValue of
                    Just queryValue -> queryValue
                        |> fromASCIIBytes
                        |> \case
                            Just uuid -> uuid |> Right
                            Nothing -> Left BadType { field = "", value = Just queryValue, expectedType = "UUID" }
                    Nothing -> Left NotMatched
                Nothing -> Left NotMatched,

            -- This has to be last parser in the list
            --
            -- Try and parse a UUID wrapped with a Id. In IHP types these are wrapped in a newtype @Id@ such as @Id User@.
            -- Since @Id@ is a newtype wrapping a UUID, it has the same data representation in GHC.
            \queryValue -> case queryValue of
                Just queryValue -> queryValue
                    |> fromASCIIBytes
                    |> \case
                        Just uuid -> uuid |> unsafeCoerce |> Right
                        Nothing -> Left BadType { field = "", value = Just queryValue, expectedType = "UUID" }
                Nothing -> Left NotMatched
            ]
{-# NOINLINE parseFuncs #-}

querySortedByFields :: Query -> Constr -> Query
querySortedByFields query constructor = constrFields constructor
        |> map cs
        |> map (\field -> (field, join $ List.lookup field query))
{-# NOINLINE querySortedByFields #-}

-- | Given a constructor and a parsed query string, attempt to construct a value of the constructor's type.
applyConstr :: (Data controller, Data idType) => (ByteString -> Maybe idType) -> Constr -> Query -> Either TypedAutoRouteError controller
applyConstr parseIdType constructor query = let

    attemptToParseArg :: forall d. (Data d) => (ByteString, Maybe ByteString) -> [Maybe ByteString -> Either TypedAutoRouteError d] -> State.StateT Query (Either TypedAutoRouteError) d
    attemptToParseArg queryParam@(queryName, queryValue) [] = State.lift (Left NoConstructorMatched
                { field = queryName
                , value = queryValue
                , expectedType = (dataTypeOf (Prelude.undefined :: d)) |> dataTypeName |> cs
                })
    attemptToParseArg queryParam@(k, v) (parseFunc:restFuncs) = case parseFunc v of
            Right result -> pure result
            Left badType@BadType{} -> State.lift (Left badType { field = k })
            Left _ -> attemptToParseArg queryParam restFuncs

    nextField :: forall d. (Data d) => State.StateT Query (Either TypedAutoRouteError) d
    nextField = do
            queryParams <- State.get
            case queryParams of
                [] -> State.lift (Left TooFewArguments)
                (p@(key, value):rest) -> do
                    State.put rest
                    attemptToParseArg p (parseFuncs parseIdType)


   in case State.runStateT (fromConstrM nextField constructor) (querySortedByFields query constructor) of
        Right (x, []) -> pure x
        Right (_) -> Left TooFewArguments
        Left e -> Left e
{-# NOINLINE applyConstr #-}

class Data controller => AutoRoute controller where
    autoRouteWithIdType :: (?request :: Request, ?respond :: Respond, Data idType) => (ByteString -> Maybe idType) -> Parser controller
    autoRouteWithIdType parseIdFunc =
        let
            query :: Query
            query = queryString ?request
        in do
            (constr, allowedMethods) <- routeMatchParser @controller
            action <- case applyConstr parseIdFunc constr query of
                    Right parsedAction -> pure parsedAction
                    Left e -> Exception.throw e
            method <- getMethod
            unless (allowedMethods |> includes method) (Exception.throw UnexpectedMethodException { allowedMethods, method })
            pure action
    {-# INLINABLE autoRouteWithIdType #-}

    autoRoute :: (?request :: Request, ?respond :: Respond) => Parser controller
    autoRoute = autoRouteWithIdType (\_ -> Nothing :: Maybe Integer)
    {-# INLINABLE autoRoute #-}

    -- | Constructs a controller value from a matched constructor and query string.
    applyAction :: Constr -> Query -> Either TypedAutoRouteError controller
    applyAction = applyConstr (\_ -> Nothing :: Maybe Integer)
    {-# INLINE applyAction #-}

    -- | Specifies the allowed HTTP methods for a given action.
    --
    -- The default heuristic mirrors the one originally shipped with
    -- 'IHP.RouterSupport':
    --
    -- * @Delete*@  — @[DELETE]@
    -- * @Update*@  — @[POST, PATCH]@
    -- * @Create*@  — @[POST]@
    -- * @Show*@    — @[GET, HEAD]@
    -- * everything else — @[GET, POST, HEAD]@
    allowedMethodsForAction :: ByteString -> [StdMethod]
    allowedMethodsForAction actionName =
            case actionName of
                a | "Delete" `ByteString.isPrefixOf` a -> [DELETE]
                a | "Update" `ByteString.isPrefixOf` a -> [POST, PATCH]
                a | "Create" `ByteString.isPrefixOf` a -> [POST]
                a | "Show"   `ByteString.isPrefixOf` a -> [GET, HEAD]
                _ -> [GET, POST, HEAD]
    {-# INLINE allowedMethodsForAction #-}

    -- | Custom route parser for overriding individual action routes.
    customRoutes :: (?request :: Request, ?respond :: Respond) => Parser controller
    customRoutes = empty
    {-# INLINE customRoutes #-}

    -- | Custom path generation for overriding individual action URLs.
    customPathTo :: controller -> Maybe Text
    customPathTo _ = Nothing
    {-# INLINE customPathTo #-}

-- | Static route-matching parser that becomes a CAF when specialized for a concrete
-- controller type. Uses a 'HashMap' for O(1) action name lookup after matching the prefix.
routeMatchParser :: forall controller. (Data controller, AutoRoute controller) => Parser (Constr, [StdMethod])
routeMatchParser = do
    string prefix
    remaining <- takeByteString
    case HashMap.lookup remaining actionMatchMap of
        Just result -> pure result
        Nothing -> fail "no matching action"
    where
        prefix :: ByteString
        prefix = Text.encodeUtf8 (actionPrefixText @controller)

        actionMatchMap :: HashMap.HashMap ByteString (Constr, [StdMethod])
        actionMatchMap = HashMap.fromList
            [ (actionPath, (constr, allowedMethods))
            | constr <- dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
            , let actionName = ByteString.pack (showConstr constr)
                  actionPath = stripActionSuffixByteString actionName
                  allowedMethods = allowedMethodsForAction @controller actionName
            ]
{-# NOINLINE routeMatchParser #-}

stripActionSuffixByteString :: ByteString -> ByteString
stripActionSuffixByteString actionName = fromMaybe actionName (ByteString.stripSuffix "Action" actionName)
{-# INLINE stripActionSuffixByteString #-}

stripActionSuffixText :: Text -> Text
stripActionSuffixText actionName = fromMaybe actionName (Text.stripSuffix "Action" actionName)
{-# INLINE stripActionSuffixText #-}

-- | Returns the create action for a given controller.
createAction :: forall controller. AutoRoute controller => Maybe controller
createAction = fmap fromConstr createConstructor
    where
        createConstructor :: Maybe Constr
        createConstructor = find isCreateConstructor allConstructors

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

        isCreateConstructor :: Constr -> Bool
        isCreateConstructor constructor = "Create" `isPrefixOf` showConstr constructor && Prelude.null (constrFields constructor)
{-# INLINE createAction #-}

-- | Returns the update action when given a controller and id.
updateAction :: forall controller id. AutoRoute controller => Maybe (id -> controller)
updateAction =
        case updateConstructor of
            Just constructor -> Just $ \id -> buildInstance constructor id
            Nothing -> Nothing
    where
        updateConstructor :: Maybe Constr
        updateConstructor = find isUpdateConstructor allConstructors

        buildInstance :: Constr -> id -> controller
        buildInstance constructor id = State.evalState ((fromConstrM (do
                i <- State.get

                State.modify (+1)
                pure (unsafeCoerce id)
            )) constructor) 0

        allConstructors :: [Constr]
        allConstructors = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))

        isUpdateConstructor :: Constr -> Bool
        isUpdateConstructor constructor = "Update" `isPrefixOf` (showConstr constructor) && (length (constrFields constructor) == 1)
{-# INLINE updateAction #-}

instance {-# OVERLAPPABLE #-} (AutoRoute controller, Controller controller) => CanRoute controller where
    parseRoute' = customRoutes <|> autoRoute
    {-# INLINABLE parseRoute' #-}

    -- | This is only used as a fallback parser (via lazy thunk in 'ControllerRouteMap').
    parseRouteWithAction toApp = (do
        action <- customRoutes @controller
        pure (toApp action)
      ) <|> (do
        (constr, allowedMethods) <- routeMatchParser @controller
        pure $ \waiRequest waiRespond -> wrapRouterException do
            case applyAction @controller constr (queryString waiRequest) of
                Left e -> Exception.throw e
                Right action -> do
                    case parseMethod (requestMethod waiRequest) of
                        Right method -> do
                            unless (allowedMethods |> includes method)
                                (Exception.throw UnexpectedMethodException { allowedMethods, method })
                            toApp action waiRequest waiRespond
                        Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
      )
    {-# INLINABLE parseRouteWithAction #-}

    -- | Override to use 'ControllerRouteMap' for O(1) HashMap dispatch.
    toControllerRoute :: forall application.
        ( ?request :: Request, ?respond :: Respond, Controller controller
        , InitControllerContext application, ?application :: application
        , Typeable application, Typeable controller
        ) => ControllerRoute application
    toControllerRoute = ControllerRouteMap
        (buildAutoRouteMap @controller @application)
        (parseRouteWithAction @controller (runAction' @application))
    {-# INLINABLE toControllerRoute #-}

-- | Instances of the @QueryParam@ type class can be represented in URLs as query parameters.
class Data a => QueryParam a where
    showQueryParam :: a -> String

instance QueryParam Text where
    showQueryParam text = Text.unpack text

instance QueryParam Int where
    showQueryParam = show

instance QueryParam Integer where
    showQueryParam = show

instance QueryParam UUID where
    showQueryParam = show

instance QueryParam a => QueryParam (Maybe a) where
    showQueryParam (Just val) = showQueryParam val
    showQueryParam Nothing = ""

instance QueryParam a => QueryParam [a] where
    showQueryParam = List.intercalate "," . map showQueryParam

instance {-# OVERLAPPABLE #-} (Show controller, AutoRoute controller) => HasPath controller where
    pathTo !action = case customPathTo action of
        Just path -> path
        Nothing ->
            let !ci = constrIndex (toConstr action) - 1
                (!basePath, !fieldNames) = constrInfoCache !! ci
            in case fieldNames of
                [] -> basePath
                _ ->
                    let !fieldValues = gmapQ renderFieldForUrl action
                    in basePath <> buildQueryText fieldNames fieldValues
        where
            constrInfoCache :: [(Text, [Text])]
            constrInfoCache = map mkInfo allConstrs
                where
                    allConstrs = dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
                    !appPrefix = actionPrefixText @controller
                    mkInfo c =
                        let !bp = appPrefix <> stripActionSuffixText (Text.pack (showConstr c))
                            !fns = map Text.pack (constrFields c)
                        in (bp, fns)

            buildQueryText :: [Text] -> [Text] -> Text
            buildQueryText names values =
                zip names values
                |> filter (\(_, v) -> not (Text.null v))
                |> map (\(k, v) -> k <> "=" <> URI.encodeText v)
                |> Text.intercalate "&"
                |> (\q -> if Text.null q then q else Text.cons '?' q)
    {-# NOINLINE pathTo #-}

-- | Render a controller field value as 'Text' for URL query parameter inclusion.
renderFieldForUrl :: forall d. Data d => d -> Text
renderFieldForUrl val
    | Just Refl <- eqT @d @UUID = toText val
    | Just Refl <- eqT @d @Text = val
    | Just Refl <- eqT @d @Int = Text.pack (show val)
    | Just Refl <- eqT @d @Integer = Text.pack (show val)
    | Just Refl <- eqT @d @(Maybe UUID) = maybe "" toText val
    | Just Refl <- eqT @d @(Maybe Text) = maybe "" id val
    | Just Refl <- eqT @d @(Maybe Int) = maybe "" (Text.pack . show) val
    | Just Refl <- eqT @d @(Maybe Integer) = maybe "" (Text.pack . show) val
    | Just Refl <- eqT @d @[UUID] = Text.intercalate "," (map toText val)
    | Just Refl <- eqT @d @[Text] = Text.intercalate "," (val :: [Text])
    | Just Refl <- eqT @d @[Int] = Text.intercalate "," (map (Text.pack . show) val)
    | Just Refl <- eqT @d @[Integer] = Text.intercalate "," (map (Text.pack . show) val)
    | otherwise =
        case gmapQ renderFieldForUrl val of
            [inner] -> inner
            _ -> ""
{-# NOINLINE renderFieldForUrl #-}

-- | Build a HashMap from full paths (prefix + action name) to Application closures.
buildAutoRouteMap :: forall controller application.
    ( AutoRoute controller
    , Controller controller
    , InitControllerContext application
    , Typeable application
    , Typeable controller
    ) => HashMap.HashMap ByteString (application -> Application)
buildAutoRouteMap = HashMap.fromList
    [ (prefix <> actionPath, handler)
    | constr <- dataTypeConstrs (dataTypeOf (Prelude.undefined :: controller))
    , let actionName = ByteString.pack (showConstr constr)
          actionPath = stripActionSuffixByteString actionName
          allowedMethods = allowedMethodsForAction @controller actionName
          handler app waiRequest waiRespond =
              let ?application = app
              in wrapRouterException do
                  case parseMethod (requestMethod waiRequest) of
                      Left err -> error ("Invalid HTTP method: " <> ByteString.unpack err)
                      Right method -> do
                          unless (allowedMethods |> includes method)
                              (Exception.throw UnexpectedMethodException { allowedMethods, method })
                          case applyAction @controller constr (queryString waiRequest) of
                              Left e -> Exception.throw e
                              Right action -> runAction' @application action waiRequest waiRespond
    ]
    where
        prefix :: ByteString
        prefix = Text.encodeUtf8 (actionPrefixText @controller)
{-# NOINLINE buildAutoRouteMap #-}

parseUUIDOrTextId :: ByteString -> Maybe Dynamic
parseUUIDOrTextId queryVal = queryVal
    |> fromASCIIBytes
    |> \case
        Just uuid -> uuid |> toDyn |> Just
        Nothing -> Nothing

parseRouteWithId
    :: forall controller application.
        ( ?request :: Request
        , ?respond :: Respond
        , CanRoute controller
        , Controller controller
        , InitControllerContext application
        , ?application :: application
        , Typeable application
        , Typeable controller
        )
    => ControllerRoute application
parseRouteWithId = parseRoute @controller @application

-- | Display a better error when the user missed to pass an argument to an action.
--
-- E.g. when you forgot to pass a projectId to the ShowProjectAction:
--
-- > <a href={ShowProjectAction}>Show project</a>
--
-- See https://github.com/digitallyinduced/ihp/issues/840
instance ((T.TypeError (T.Text "Looks like you forgot to pass a " :<>: (T.ShowType argument) :<>: T.Text " to this " :<>: (T.ShowType controller))), Data argument, Data controller, Data (argument -> controller)) => AutoRoute (argument -> controller) where
