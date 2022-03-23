{-|
Module: IHP.GraphQL.GraphQLWS
Description: Implements a WebSocket server for graphql-ws as described in https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.GraphQL.GraphQLWS where

import IHP.Prelude
import IHP.GraphQL.Types
import qualified Data.Attoparsec.ByteString.Char8 as Attoparsec

import IHP.ApplicationContext (ApplicationContext)
import qualified IHP.ApplicationContext as ApplicationContext
import IHP.ControllerPrelude hiding (Error)
import Network.Wai
import qualified Network.Wai.Handler.WebSockets as WebSockets
import qualified Network.WebSockets as WebSockets
import qualified IHP.WebSocket as WebSockets
import qualified Network.HTTP.Types as HTTP
import qualified IHP.Controller.Context as Context
import qualified IHP.Controller.RequestContext
import qualified IHP.Log as Log
import qualified Control.Exception as Exception

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding.Internal as Aeson

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Control.Concurrent.MVar as MVar

import qualified Database.PostgreSQL.Simple as PG

import qualified IHP.GraphQL.Types as GraphQL
import qualified IHP.GraphQL.Parser as GraphQL
import qualified IHP.GraphQL.Compiler as GraphQL
import qualified IHP.GraphQL.Analysis as GraphQL
import qualified IHP.GraphQL.Patch as GraphQL
import IHP.GraphQL.JSON ()
import qualified Data.Attoparsec.Text as AttoparsecText
import IHP.DataSync.RowLevelSecurity
import IHP.DataSync.DynamicQuery
import IHP.DataSync.REST.Controller ()
import IHP.DataSync.Controller (changesToValue)
import qualified IHP.DataSync.ChangeNotifications as ChangeNotifications
import Data.Set (Set)
import qualified Data.Set as Set
import qualified IHP.PGListener as PGListener
import qualified Web.JWT as JWT
import qualified Data.UUID as UUID

-- | Cannot be implemented natively in IHP as we need to accept the @graphql-transport-ws@ sub protocol
routeGraphQLWS ::
    ( ?application :: application
    , ?applicationContext :: ApplicationContext
    , ?context :: RequestContext
    ) => Attoparsec.Parser (IO ResponseReceived)
routeGraphQLWS = do
    Attoparsec.string "/graphql-ws"
    Attoparsec.endOfInput
    
    let ?modelContext = ApplicationContext.modelContext ?applicationContext
    let ?requestContext = ?context
    let respond = ?context |> get #respond
    let request = ?context |> get #request
    let acceptRequest = WebSockets.AcceptRequest { acceptSubprotocol = "graphql-transport-ws", acceptHeaders = [] }

    let handleConnection pendingConnection = do
            connection <- WebSockets.acceptRequestWith pendingConnection acceptRequest

            controllerContext <- Context.newControllerContext
            let ?context = controllerContext

            WebSockets.startWSApp @GraphQLWSApp connection

    pure $ request
        |> WebSockets.websocketsApp WebSockets.defaultConnectionOptions handleConnection
        |> \case
            Just response -> respond response
            Nothing -> respond $ responseLBS HTTP.status400 [(HTTP.hContentType, "text/plain")] "This endpoint is only available via a WebSocket"

data GraphQLWSApp = GraphQLWSApp
        { subscriptions :: !(HashMap UUID (MVar.MVar ()))
        , asyncs :: ![Async ()]
        }

-- | Messages according to https://github.com/enisdenjo/graphql-ws/blob/master/PROTOCOL.md
data Message
    = ConnectionInit { connectionInitPayload :: HashMap Text Aeson.Value } -- ^ Direction: Client -> Server
    | ConnectionAck -- ^ Direction: Server -> Client
    | Ping -- ^ Direction: bidirectional
    | Pong -- ^ Direction: bidirectional
    | Subscribe
        { id :: !UUID
        , operationName :: !(Maybe Text)
        , query :: !Text
        , variables :: !(Maybe GraphQL.Variables)
        , extensions :: !(Maybe Aeson.Value)
        } -- ^ Direction: Client -> Server
    | Next { id :: !UUID, nextPayload :: UndecodedJSON } -- ^ Direction: Server -> Client
    | Error { id :: !UUID, errorPayload :: [Text] } -- ^ Direction: Server -> Client
    | Complete { id :: !UUID } -- ^ Direction: bidirectional
    deriving (Show)

instance WSApp GraphQLWSApp where
    initialState = GraphQLWSApp { subscriptions = HashMap.empty, asyncs = [] }

    run = do
        userIdVar <- newIORef Nothing
        ensureRLSEnabled <- makeCachedEnsureRLSEnabled
        installTableChangeTriggers <- ChangeNotifications.makeCachedInstallTableChangeTriggers
        let pgListener = ?applicationContext |> get #pgListener

        forever do
            message <- Aeson.eitherDecodeStrict' <$> receiveData @ByteString

            case message of
                Right decodedMessage -> do
                    Exception.mask \restore -> do
                        -- Handle the messages in an async way
                        -- This increases throughput as multiple queries can be fetched
                        -- in parallel
                        handlerProcess <- async $ restore do
                            result <- Exception.try (handleMessage userIdVar ensureRLSEnabled installTableChangeTriggers pgListener decodedMessage)

                            case result of
                                Left (e :: Exception.SomeException) -> do
                                    let errorMessage = case fromException e of
                                            Just (enhancedSqlError :: EnhancedSqlError) -> cs (get #sqlErrorMsg (get #sqlError enhancedSqlError))
                                            Nothing -> cs (displayException e)
                                    Log.error (tshow e)
                                    error errorMessage
                                Right result -> pure ()

                        modifyIORef' ?state (\state -> state |> modify #asyncs (handlerProcess:))
                        pure ()
                Left errorMessage -> error ("Invalid message: " <> cs errorMessage)

    onClose = cleanupAllSubscriptions

cleanupAllSubscriptions :: _ => (?state :: IORef GraphQLWSApp, ?applicationContext :: ApplicationContext) => IO ()
cleanupAllSubscriptions = do
    state <- getState

    case state of
        GraphQLWSApp { asyncs } -> forEach asyncs uninterruptibleCancel
        _ -> pure ()

handleMessage :: (?state :: IORef GraphQLWSApp, ?connection :: WebSockets.Connection, ?modelContext :: ModelContext, ?context :: ControllerContext) => IORef (Maybe UUID) -> _ -> _ -> _ -> Message -> IO ()
handleMessage userIdVar _ _ _ Ping = sendJSON Pong
handleMessage userIdVar _ _ _ ConnectionInit { connectionInitPayload } = do
    initAuth userIdVar connectionInitPayload
    sendJSON ConnectionAck
handleMessage userIdVar ensureRLSEnabled installTableChangeTriggers pgListener Subscribe { id, operationName, query, variables, extensions } =
    let
        handleEnhancedSqlError (exception :: EnhancedSqlError) = sendJSON Error { id = id, errorPayload = [ cs $ get #sqlErrorMsg (get #sqlError exception) ] }
        handleSomeException (exception :: SomeException) = sendJSON Error { id = id, errorPayload = [ tshow exception ] }

        handleError :: IO () -> IO ()
        handleError inner = (inner `Exception.catch` handleEnhancedSqlError) `catch` handleSomeException
    in handleError do
        let document = case AttoparsecText.parseOnly GraphQL.parseDocument query of
                Left parserError -> error (cs $ tshow parserError)
                Right statements -> statements

        tablesRLS <- ensureRLSEnabledForGraphQLDocument ensureRLSEnabled document

        let emptyVariables = GraphQL.Variables []
        let [(theQuery, theParams)] = GraphQL.compileDocument (fromMaybe emptyVariables variables) document

        userId <- readIORef userIdVar
        [PG.Only (graphQLResult :: UndecodedJSON)] <- sqlQueryWithRLS' userId theQuery theParams

        if GraphQL.isSubscriptionDocument document
            then do
                ensureBelowSubscriptionsLimit

                let (UndecodedJSON graphQLResultText) = graphQLResult
                let (Just decodedGraphQLResult) = Aeson.decode (cs graphQLResultText)

                -- We keep an in-memory version of the result to apply db changes to
                graphVar <- newIORef decodedGraphQLResult

                -- We need to keep track of all the ids of entities we're watching to make
                -- sure that we only send update notifications to clients that can actually
                -- access the record (e.g. if a RLS policy denies access)
                let watchedRecordIds = GraphQL.recordIds document decodedGraphQLResult

                -- Store it in IORef as an INSERT requires us to add an id
                watchedRecordIdsRef <- newIORef watchedRecordIds

                -- Make sure the database triggers are there
                forEach tablesRLS installTableChangeTriggers
                
                let callback table notification = case notification of
                            ChangeNotifications.DidInsert { id } -> do
                                -- The new record could not be accessible to the current user with a RLS policy
                                -- E.g. it could be a new record in a 'projects' table, but the project belongs
                                -- to a different user, and thus the current user should not be able to see it.
                                --
                                -- The new record could also be not part of the WHERE condition of the initial query.
                                -- Therefore we need to use the subscriptions WHERE condition to fetch the new record here.
                                --
                                -- To honor the RLS policies we therefore need to fetch the record as the current user
                                -- If the result set is empty, we know the record is not accesible to us
                                [PG.Only (UndecodedJSON graphQLResultText)] <- sqlQueryWithRLS' userId theQuery theParams
                                let (Just graphQLResult) = Aeson.decode (cs graphQLResultText)

                                case GraphQL.extractRecordById id graphQLResult of
                                    Just (Aeson.Object newRecord) -> do
                                        -- Add the new record to 'watchedRecordIdsRef'
                                        -- Otherwise the updates and deletes will not be dispatched to the client
                                        modifyIORef' watchedRecordIdsRef (HashMap.adjust (Set.insert id) table)

                                        modifyIORef' graphVar (GraphQL.insertRecord table id newRecord document)

                                        nextPayload <- UndecodedJSON . cs .Aeson.encode <$> readIORef graphVar
                                        sendJSON Next { id, nextPayload }
                                    _ -> pure ()
                            ChangeNotifications.DidUpdate { id, changeSet } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- Set.member id . HashMap.lookupDefault Set.empty table <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    let (Aeson.Object patch) = changesToValue changeSet
                                    modifyIORef' graphVar (GraphQL.updateRecord table id patch document)

                                    nextPayload <- UndecodedJSON . cs . Aeson.encode <$> readIORef graphVar
                                    sendJSON Next { id, nextPayload }
                            ChangeNotifications.DidDelete { id } -> do
                                -- Only send the notifcation if the deleted record was part of the initial
                                -- results set
                                isWatchingRecord <- Set.member id . HashMap.lookupDefault Set.empty table <$> readIORef watchedRecordIdsRef
                                when isWatchingRecord do
                                    modifyIORef' graphVar (GraphQL.deleteRecord table id document)
                                    nextPayload <- UndecodedJSON . cs . Aeson.encode <$> readIORef graphVar
                                    sendJSON Next { id, nextPayload }

                let startWatchers tablesRLS = case tablesRLS of
                        (tableNameRLS:rest) -> do
                            let subscribe = PGListener.subscribeJSON (ChangeNotifications.channelName tableNameRLS) (callback (get #tableName tableNameRLS)) pgListener
                            let unsubscribe subscription = PGListener.unsubscribe subscription pgListener

                            Exception.bracket subscribe unsubscribe (\_ -> startWatchers rest)
                        [] -> do
                            close <- MVar.newEmptyMVar
                            modifyIORef' ?state (\state -> state |> modify #subscriptions (HashMap.insert id close))

                            sendJSON Next { id, nextPayload = graphQLResult }

                            MVar.takeMVar close

                startWatchers tablesRLS
            else do
                sendJSON Next { id, nextPayload = graphQLResult }
                sendJSON Complete { id }

        pure ()
handleMessage _ _ _ _ message = do
    putStrLn (tshow message)

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> do
        type_ :: Text <- v .: "type"

        case type_ of
            "connection_init" -> do
                payload <- v .: "payload"
                pure ConnectionInit { connectionInitPayload = payload }
            "ping" -> pure Ping
            "pong" -> pure Pong
            "subscribe" -> do
                id <- v .: "id"
                payload <- v .: "payload"
                operationName <- payload .:? "operationName"
                query <- payload .: "query"
                variables <- payload .:? "variables"
                extensions <- payload .:? "extensions"
                pure Subscribe { id, operationName, query, variables, extensions }
            "complete" -> do
                id <- v .: "id"
                pure Complete { id }
            type_ -> fail "Invalid type"

instance ToJSON Message where
    toJSON ConnectionAck = object [ "type" .= ("connection_ack" :: Text) ]
    toJSON Ping = object [ "type" .= ("ping" :: Text) ]
    toJSON Pong = object [ "type" .= ("pong" :: Text) ]
    toJSON Next { id, nextPayload } = object [ "type" .= ("next" :: Text), "id" .= id, "payload" .= nextPayload ]
    toJSON Error { id, errorPayload } = object [ "type" .= ("error" :: Text), "id" .= id, "payload" .= errorPayload ]
    toJSON Complete { id } = object [ "type" .= ("complete" :: Text), "id" .= id ]
    
    toEncoding ConnectionAck = Aeson.unsafeToEncoding "{\"type\":\"connection_ack\"}"
    toEncoding Ping = Aeson.unsafeToEncoding "{\"type\":\"ping\"}"
    toEncoding Pong = Aeson.unsafeToEncoding "{\"type\":\"pong\"}"
    toEncoding Next { id, nextPayload } = Aeson.econcat
        [ Aeson.unsafeToEncoding "{\"type\":\"next\",\"id\":"
        , Aeson.toEncoding id
        , Aeson.unsafeToEncoding ",\"payload\":"
        , toEncoding nextPayload
        , Aeson.unsafeToEncoding "}"
        ]
    toEncoding Error { id, errorPayload } = Aeson.econcat
        [ Aeson.unsafeToEncoding "{\"type\":\"error\",\"id\":"
        , Aeson.toEncoding id
        , Aeson.unsafeToEncoding ",\"payload\":"
        , toEncoding errorPayload
        , Aeson.unsafeToEncoding "}"
        ]
    toEncoding Complete { id } = Aeson.econcat
        [ Aeson.unsafeToEncoding "{\"type\":\"complete\",\"id\":"
        , Aeson.toEncoding id
        , Aeson.unsafeToEncoding "}"
        ]


instance SetField "subscriptions" GraphQLWSApp (HashMap UUID (MVar.MVar ())) where
    setField subscriptions record = record { subscriptions }

instance SetField "asyncs" GraphQLWSApp [Async ()] where
    setField asyncs record = record { asyncs }

ensureRLSEnabledForGraphQLDocument :: _ -> GraphQL.Document -> IO [TableWithRLS]
ensureRLSEnabledForGraphQLDocument ensureRLSEnabled document = do
    let tables = document
            |> GraphQL.tablesUsedInDocument
            |> Set.toList
    mapM ensureRLSEnabled tables

ensureBelowSubscriptionsLimit :: (?state :: IORef GraphQLWSApp, ?context :: ControllerContext) => IO ()
ensureBelowSubscriptionsLimit = do
    subscriptions <- get #subscriptions <$> readIORef ?state
    let subscriptionsCount = HashMap.size subscriptions
    when (subscriptionsCount >= maxSubscriptionsPerConnection) do
        error ("You've reached the subscriptions limit of " <> tshow maxSubscriptionsPerConnection <> " subscriptions")

maxSubscriptionsPerConnection :: _ => Int
maxSubscriptionsPerConnection = 
    case getAppConfig @DataSyncMaxSubscriptionsPerConnection of
        DataSyncMaxSubscriptionsPerConnection value -> value

initAuth userIdVar options = do
    let jwt = HashMap.lookup "jwt" options
    case jwt of
        Just (Aeson.String jwt) -> loginWithJWT userIdVar jwt
        otherwise -> pure ()

loginWithJWT userIdVar jwt = do
    let signature = JWT.decodeAndVerifySignature (getAppConfig @JWT.Signer) jwt

    case signature of
        Just jwt -> do
            let userId = jwt
                    |> JWT.claims
                    |> JWT.sub
                    |> fromMaybe (error "JWT missing sub")
                    |> JWT.stringOrURIToText
                    |> UUID.fromText

            writeIORef userIdVar userId
        Nothing -> error "Invalid signature"