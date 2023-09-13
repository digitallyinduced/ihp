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

data CompletionRequest = CompletionRequest
    { messages :: ![Message]
    , prompt :: !Text
    , maxTokens :: !Int
    , temperature :: !Double
    , presencePenalty :: !Double
    , frequencePenalty :: !Double
    , model :: !Text
    }

data Message = Message
    { role :: !Role
    , content :: !Text
    }

data Role = UserRole | SystemRole | AssistantRole

instance ToJSON CompletionRequest where
    toJSON CompletionRequest { model, prompt, messages, maxTokens, temperature, presencePenalty, frequencePenalty } =
        object
            [ "model" .= model
            , "messages" .= (messages <> [userMessage prompt])
            , "max_tokens" .= maxTokens
            , "stream" .= True
            , "temperature" .= temperature
            , "presence_penalty" .= presencePenalty
            , "frequency_penalty" .= frequencePenalty
            ]

instance ToJSON Role where
    toJSON UserRole = toJSON ("user" :: Text)
    toJSON SystemRole = toJSON ("system" :: Text)
    toJSON AssistantRole = toJSON ("assistant" :: Text)

instance ToJSON Message where
    toJSON Message { role, content } =
        object [ "role" .= role, "content" .= content ]

userMessage :: Text -> Message
userMessage content = Message { role = UserRole, content }

systemMessage :: Text -> Message
systemMessage content = Message { role = SystemRole, content }

newCompletionRequest :: CompletionRequest
newCompletionRequest = CompletionRequest
    { prompt = ""
    , messages = []
    , maxTokens = 0
    , temperature = 0.5
    , presencePenalty = 2
    , frequencePenalty = 0.2
    , model = "gpt-3.5-turbo"
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
        delta <- v .: "delta"
        content <- delta .: "content"
        pure Choice { text = content }


streamCompletion :: ByteString -> CompletionRequest -> IO () -> (Text -> IO ()) -> IO Text
streamCompletion secretKey completionRequest onStart callback = do
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
            modifyIORef' completionRequestRef (\completionRequest -> completionRequest { prompt = completionRequest.prompt <> text, maxTokens = completionRequest.maxTokens - (length (Text.words text)) })
            callback text

        retryPolicyDefault = Retry.constantDelay 50000 <> Retry.limitRetries 10

streamCompletionWithoutRetry :: ByteString -> CompletionRequest -> IO () -> (Text -> IO ()) -> IO (Either Text Text)
streamCompletionWithoutRetry secretKey completionRequest onStart callback = do
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

                    let handler = \p i -> do
                            let status = getStatusCode p
                            if status == 200
                                then do
                                    x <- Streams.foldM (parseResponseChunk callback) ("", "") i
                                    return (Right (snd x))
                                else do
                                    x <- Streams.fold mappend mempty i
                                    return (Left $ "an error happend: " <> Text.pack (show x))

                    onStart
                    receiveResponse connection handler
    where
        parseResponseChunk :: (Text -> IO ()) -> (ByteString, Text) -> ByteString -> IO (ByteString, Text)
        parseResponseChunk callback (curBuffer, chunk) input = do
            case ByteString.stripPrefix "data: " (ByteString.strip (curBuffer <> input)) of
                Just json -> do
                    case decodeStrict json of
                        Just CompletionResult { choices } -> do
                            let tokens :: Text = mconcat $ map (.text) choices
                            callback tokens
                            pure ("", chunk <> tokens)
                        otherwise -> do
                            pure (curBuffer <> json, chunk)
                Nothing -> pure (curBuffer <> input, chunk)
