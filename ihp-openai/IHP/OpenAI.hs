module IHP.OpenAI where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Exception (SomeException)
import Data.IORef

import qualified System.IO.Streams as Streams
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString
import Network.Http.Client
import Data.Aeson
import OpenSSL
import qualified OpenSSL.Session as SSL
import qualified Data.Text as Text
import qualified Control.Retry as Retry
import qualified Control.Exception as Exception
import Control.Applicative ((<|>))

data CompletionRequest = CompletionRequest
    { messages :: ![Message]
    , model :: !Text
    , maxTokens :: !(Maybe Int)
    , temperature :: !(Maybe Double)
    , presencePenalty :: !(Maybe Double)
    , frequencePenalty :: !(Maybe Double)
    , stream :: !Bool
    , responseFormat :: !(Maybe ResponseFormat)
    } deriving (Eq, Show)

data Message = Message
    { role :: !Role
    , content :: !Text
    } deriving (Eq, Show)

data Role
    = UserRole
    | SystemRole
    | AssistantRole
    deriving (Eq, Show)

data ResponseFormat
    = ResponseFormat { type_ :: !ResponseFormatType }
    deriving (Eq, Show)

data ResponseFormatType
    = Text
    | JsonObject
    deriving (Eq, Show)

instance ToJSON CompletionRequest where
    toJSON CompletionRequest { model, messages, maxTokens, temperature, presencePenalty, frequencePenalty, stream, responseFormat } =
        object
            [ "model" .= model
            , "messages" .= messages
            , "max_tokens" .= maxTokens
            , "stream" .= stream
            , "temperature" .= temperature
            , "presence_penalty" .= presencePenalty
            , "frequency_penalty" .= frequencePenalty
            , "response_format" .= responseFormat
            ]

instance ToJSON Role where
    toJSON UserRole = toJSON ("user" :: Text)
    toJSON SystemRole = toJSON ("system" :: Text)
    toJSON AssistantRole = toJSON ("assistant" :: Text)

instance ToJSON Message where
    toJSON Message { role, content } =
        object [ "role" .= role, "content" .= content ]

instance ToJSON ResponseFormat where
    toJSON ResponseFormat { type_ } =
        object [ "type" .= type_ ]

instance ToJSON ResponseFormatType where
    toJSON Text = toJSON ("text" :: Text)
    toJSON JsonObject = toJSON ("json_object" :: Text)

userMessage :: Text -> Message
userMessage content = Message { role = UserRole, content }

systemMessage :: Text -> Message
systemMessage content = Message { role = SystemRole, content }

assistantMessage :: Text -> Message
assistantMessage content = Message { role = AssistantRole, content }

newCompletionRequest :: CompletionRequest
newCompletionRequest = CompletionRequest
    { messages = []
    , maxTokens = Nothing
    , temperature = Nothing
    , presencePenalty = Nothing
    , frequencePenalty = Nothing
    , model = "gpt-3.5-turbo"
    , stream = False
    , responseFormat = Nothing
    }

data CompletionResult = CompletionResult
    { choices :: [Choice]
    }

instance FromJSON CompletionResult where
    parseJSON = withObject "CompletionResult" $ \v -> CompletionResult
        <$> v .: "choices"

-- [{"text": "Introdu", "index": 0, "logprobs": null, "finish_reason": null}]
data Choice = Choice
    { text :: !Text
    }

instance FromJSON Choice where
    parseJSON = withObject "Choice" $ \v -> do
        deltaOrMessage <- (v .: "message") <|> (v .: "delta")
        content <- deltaOrMessage .: "content"
        pure Choice { text = content }


streamCompletion :: ByteString -> CompletionRequest -> IO () -> (Text -> IO ()) -> IO Text
streamCompletion secretKey completionRequest' onStart callback = do
        let completionRequest = enableStream completionRequest'
        completionRequestRef <- newIORef completionRequest
        result <- Retry.retrying retryPolicyDefault shouldRetry (action completionRequestRef)
        case result of
            Left (e :: SomeException) -> Exception.throwIO e
            Right (Left e) -> error (Text.unpack e)
            Right (Right r) -> pure r
    where
        shouldRetry retryStatus (Left e) = pure True
        shouldRetry retryStatus (Right (Left _)) = pure True
        shouldRetry retryStatus (Right (Right r)) = pure False
        action completionRequestRef retryStatus = do
            completionRequest <- readIORef completionRequestRef
            let onStart' = if retryStatus.rsIterNumber == 0 then onStart else pure ()
            Exception.try (streamCompletionWithoutRetry secretKey completionRequest onStart' (wrappedCallback completionRequestRef))

        wrappedCallback completionRequestRef text = do
            modifyIORef' completionRequestRef (\completionRequest -> completionRequest
                    { messages = completionRequest.messages <> [assistantMessage text]
                    , maxTokens = case completionRequest.maxTokens of
                        Just maxTokens -> Just $ maxTokens - (length (Text.words text))
                        Nothing -> Nothing
                    }
                )
            callback text

        retryPolicyDefault = Retry.constantDelay 50000 <> Retry.limitRetries 10

streamCompletionWithoutRetry :: ByteString -> CompletionRequest -> IO () -> (Text -> IO ()) -> IO (Either Text Text)
streamCompletionWithoutRetry secretKey completionRequest' onStart callback = do
    let completionRequest = enableStream completionRequest'
    modifyContextSSL (\context -> do
            SSL.contextSetVerificationMode context SSL.VerifyNone
            pure context
        )
    withOpenSSL do
        withConnection (establishConnection "https://api.openai.com/v1/chat/completions") \connection -> do
            let q = buildRequest1 do
                    http POST "/v1/chat/completions"
                    setContentType "application/json"
                    Network.Http.Client.setHeader "Authorization" ("Bearer " <> secretKey)
            sendRequest connection q (jsonBody completionRequest)
            onStart
            receiveResponse connection handler

    where
        handler :: Response -> Streams.InputStream ByteString -> IO (Either Text Text)
        handler response stream = do
            let status = getStatusCode response
            if status == 200
                then do
                    {-
                    parse stream line by line as event stream format according to API spec:
                    https://platform.openai.com/docs/api-reference/chat/create#chat/create-stream
                    https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events/Using_server-sent_events#event_stream_format
                    -}
                    (_, _, output) <- Streams.lines stream >>= Streams.foldM (parseResponseChunk callback) ("", False, "")
                    return (Right output)
                else do
                    x :: ByteString <- Streams.fold mappend mempty stream
                    return (Left $ "an error happend: " <> Text.pack (show x))

        parseResponseChunk :: (Text -> IO ()) -> (ByteString, Bool, Text) -> ByteString -> IO (ByteString, Bool, Text)
        parseResponseChunk callback (curBuffer, emptyLineFound, chunk) input
            -- input line is empty, but previous was not, append newline to buffer
            | ByteString.null input && not emptyLineFound = pure (curBuffer <> "\n", True, chunk)
            -- input line is empty, previous line was already empty: message ended, clear buffer
            | ByteString.null input && emptyLineFound = pure ("", True, chunk)
            -- lines starting with : are comments, ignore
            | ":" `ByteString.isPrefixOf` input = pure (curBuffer, False, chunk)
            -- try to parse line together with buffer otherwise
            | otherwise = case ByteString.stripPrefix "data: " (ByteString.strip (curBuffer <> input)) of
                    Just json -> do
                        case eitherDecodeStrict json of
                            Right CompletionResult { choices } -> do
                                let tokens :: Text = mconcat $ map (.text) choices
                                callback tokens
                                pure ("", False, chunk <> tokens)
                            Left err -> pure (curBuffer <> json, False, chunk)
                    Nothing -> pure (curBuffer <> input, False, chunk)


fetchCompletion :: ByteString -> CompletionRequest -> IO Text
fetchCompletion secretKey completionRequest = do
        result <- Retry.retrying retryPolicyDefault shouldRetry action
        case result of
            Left (e :: SomeException) -> Exception.throwIO e
            Right result -> pure result
    where
        shouldRetry retryStatus (Left _) = pure True
        shouldRetry retryStatus (Right _) = pure False
        action retryStatus = Exception.try (fetchCompletionWithoutRetry secretKey completionRequest)

        retryPolicyDefault = Retry.constantDelay 50000 <> Retry.limitRetries 10

fetchCompletionWithoutRetry :: ByteString -> CompletionRequest -> IO Text
fetchCompletionWithoutRetry secretKey completionRequest = do
        modifyContextSSL (\context -> do
                SSL.contextSetVerificationMode context SSL.VerifyNone
                pure context
            )
        withOpenSSL do
            withConnection (establishConnection "https://api.openai.com/v1/chat/completions") \connection -> do
                    let q = buildRequest1 do
                                http POST "/v1/chat/completions"
                                setContentType "application/json"
                                Network.Http.Client.setHeader "Authorization" ("Bearer " <> secretKey)

                    sendRequest connection q (jsonBody completionRequest)
                    completionResult :: CompletionResult <- receiveResponse connection jsonHandler
                    pure (mconcat $ map (.text) completionResult.choices)

enableStream :: CompletionRequest -> CompletionRequest
enableStream completionRequest = completionRequest { stream = True }
