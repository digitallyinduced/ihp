{-|
Module: Network.Wai.Middleware.ContentSecurityPolicy
Description: Type-safe Content Security Policy (CSP) middleware for WAI
Copyright: (c) digitally induced GmbH, 2025

This module provides a WAI middleware for setting Content Security Policy headers.
-}
module Network.Wai.Middleware.ContentSecurityPolicy
    ( -- * Middleware
      cspMiddleware
    , cspMiddlewareWithNonce
    
    -- * CSP Types
    , CSP (..)
    , CSPDirective (..)
    , CSPSource (..)
    , CSPSourceList
    
    -- * Predefined Policies
    , defaultCSP
    , strictCSP
    
    -- * CSP Source Helpers
    , nonce
    , self
    , unsafeInline
    , unsafeEval
    , strictDynamic
    , none
    , data'
    , https
    , http
    , blob
    , mediastream
    , filesystem
    , host
    , scheme
    
    -- * Rendering
    , renderCSP
    , renderCSPDirective
    , renderCSPSource
    
    -- * Nonce Support
    , CSPNonce(..)
    , generateNonce
    , cspNonceKey
    ) where

import Prelude
import Network.Wai
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.Text.Encoding as Text
import qualified Data.Vault.Lazy as Vault
import qualified Data.ByteString.Base64 as Base64
import System.Random (randomIO)
import qualified Data.ByteString as BS
import Data.Word (Word8)
import Data.String (IsString(..))
import System.IO.Unsafe (unsafePerformIO)

-- | Represents a Content Security Policy
data CSP = CSP
    { defaultSrc :: Maybe CSPSourceList
    , scriptSrc :: Maybe CSPSourceList
    , styleSrc :: Maybe CSPSourceList
    , imgSrc :: Maybe CSPSourceList
    , connectSrc :: Maybe CSPSourceList
    , fontSrc :: Maybe CSPSourceList
    , objectSrc :: Maybe CSPSourceList
    , mediaSrc :: Maybe CSPSourceList
    , frameSrc :: Maybe CSPSourceList
    , frameAncestors :: Maybe CSPSourceList
    , baseUri :: Maybe CSPSourceList
    , formAction :: Maybe CSPSourceList
    , manifestSrc :: Maybe CSPSourceList
    , workerSrc :: Maybe CSPSourceList
    , childSrc :: Maybe CSPSourceList
    , reportUri :: Maybe Text
    , reportTo :: Maybe Text
    , upgradeInsecureRequests :: Bool
    , blockAllMixedContent :: Bool
    } deriving (Show, Eq)

-- | CSP directive names
data CSPDirective
    = DefaultSrc
    | ScriptSrc
    | StyleSrc
    | ImgSrc
    | ConnectSrc
    | FontSrc
    | ObjectSrc
    | MediaSrc
    | FrameSrc
    | FrameAncestors
    | BaseUri
    | FormAction
    | ManifestSrc
    | WorkerSrc
    | ChildSrc
    | ReportUri
    | ReportTo
    | UpgradeInsecureRequests
    | BlockAllMixedContent
    deriving (Show, Eq)

-- | CSP source values
data CSPSource
    = Self
    | UnsafeInline
    | UnsafeEval
    | StrictDynamic
    | None
    | Nonce Text
    | Hash Text Text  -- algorithm and hash value
    | Host Text
    | Scheme Text
    | Data
    | Https
    | Http
    | Blob
    | Mediastream
    | Filesystem
    deriving (Show, Eq)

-- | A list of CSP sources for a directive
type CSPSourceList = [CSPSource]

-- | A newtype wrapper for CSP nonces stored in the request vault
newtype CSPNonce = CSPNonce Text
    deriving (Show, Eq)

-- | Vault key for storing CSP nonce in request
-- 
-- Note: Uses unsafePerformIO to create a global vault key. This is a common
-- pattern for vault keys and is safe when combined with NOINLINE pragma.
-- The key is created once and reused across all requests.
cspNonceKey :: Vault.Key CSPNonce
cspNonceKey = unsafePerformIO Vault.newKey
{-# NOINLINE cspNonceKey #-}

-- | Generate a cryptographically secure nonce for CSP
generateNonce :: IO Text
generateNonce = do
    bytes <- sequence [randomIO :: IO Word8 | _ <- [1..32]]
    pure $ Text.decodeUtf8 $ Base64.encode $ BS.pack bytes

-- | WAI middleware that adds CSP header with a fresh nonce for each request
-- The nonce is stored in the request vault and can be retrieved using cspNonceKey
cspMiddlewareWithNonce :: (Text -> CSP) -> Middleware
cspMiddlewareWithNonce cspBuilder app req respond = do
    nonceValue <- generateNonce
    let csp = cspBuilder nonceValue
    let cspHeader = ("Content-Security-Policy", Text.encodeUtf8 $ renderCSP csp)
    let req' = req { vault = Vault.insert cspNonceKey (CSPNonce nonceValue) req.vault }
    
    app req' $ \response ->
        respond $ mapResponseHeaders (\headers -> cspHeader : headers) response

-- | WAI middleware that adds CSP header
cspMiddleware :: CSP -> Middleware
cspMiddleware csp app req respond = do
    let cspHeader = ("Content-Security-Policy", Text.encodeUtf8 $ renderCSP csp)
    
    app req $ \response ->
        respond $ mapResponseHeaders (\headers -> cspHeader : headers) response

-- | Default CSP with safe defaults
defaultCSP :: CSP
defaultCSP = CSP
    { defaultSrc = Just [self]
    , scriptSrc = Nothing
    , styleSrc = Nothing
    , imgSrc = Nothing
    , connectSrc = Nothing
    , fontSrc = Nothing
    , objectSrc = Just [none]
    , mediaSrc = Nothing
    , frameSrc = Nothing
    , frameAncestors = Nothing
    , baseUri = Just [none]
    , formAction = Nothing
    , manifestSrc = Nothing
    , workerSrc = Nothing
    , childSrc = Nothing
    , reportUri = Nothing
    , reportTo = Nothing
    , upgradeInsecureRequests = False
    , blockAllMixedContent = False
    }

-- | Strict CSP with nonce-based script and style loading
strictCSP :: Text -> CSP
strictCSP nonceValue = CSP
    { defaultSrc = Just [self]
    , scriptSrc = Just [nonce nonceValue, strictDynamic]
    , styleSrc = Just [nonce nonceValue, self]
    , imgSrc = Just [self, data']
    , connectSrc = Just [self]
    , fontSrc = Just [self]
    , objectSrc = Just [none]
    , mediaSrc = Just [self]
    , frameSrc = Nothing
    , frameAncestors = Just [self]
    , baseUri = Just [none]
    , formAction = Just [self]
    , manifestSrc = Nothing
    , workerSrc = Nothing
    , childSrc = Nothing
    , reportUri = Nothing
    , reportTo = Nothing
    , upgradeInsecureRequests = False
    , blockAllMixedContent = False
    }

-- | Helper functions to create CSP sources

self :: CSPSource
self = Self

unsafeInline :: CSPSource
unsafeInline = UnsafeInline

unsafeEval :: CSPSource
unsafeEval = UnsafeEval

strictDynamic :: CSPSource
strictDynamic = StrictDynamic

none :: CSPSource
none = None

data' :: CSPSource
data' = Data

https :: CSPSource
https = Https

http :: CSPSource
http = Http

blob :: CSPSource
blob = Blob

mediastream :: CSPSource
mediastream = Mediastream

filesystem :: CSPSource
filesystem = Filesystem

nonce :: Text -> CSPSource
nonce = Nonce

host :: Text -> CSPSource
host = Host

scheme :: Text -> CSPSource
scheme = Scheme

-- | Render a CSPSource to its text representation
renderCSPSource :: CSPSource -> Text
renderCSPSource Self = "'self'"
renderCSPSource UnsafeInline = "'unsafe-inline'"
renderCSPSource UnsafeEval = "'unsafe-eval'"
renderCSPSource StrictDynamic = "'strict-dynamic'"
renderCSPSource None = "'none'"
renderCSPSource (Nonce value) = "'nonce-" <> value <> "'"
renderCSPSource (Hash algo value) = "'" <> algo <> "-" <> value <> "'"
renderCSPSource (Host h) = h
renderCSPSource (Scheme s) = s
renderCSPSource Data = "data:"
renderCSPSource Https = "https:"
renderCSPSource Http = "http:"
renderCSPSource Blob = "blob:"
renderCSPSource Mediastream = "mediastream:"
renderCSPSource Filesystem = "filesystem:"

-- | Render a CSP directive name to its text representation
renderCSPDirective :: CSPDirective -> Text
renderCSPDirective DefaultSrc = "default-src"
renderCSPDirective ScriptSrc = "script-src"
renderCSPDirective StyleSrc = "style-src"
renderCSPDirective ImgSrc = "img-src"
renderCSPDirective ConnectSrc = "connect-src"
renderCSPDirective FontSrc = "font-src"
renderCSPDirective ObjectSrc = "object-src"
renderCSPDirective MediaSrc = "media-src"
renderCSPDirective FrameSrc = "frame-src"
renderCSPDirective FrameAncestors = "frame-ancestors"
renderCSPDirective BaseUri = "base-uri"
renderCSPDirective FormAction = "form-action"
renderCSPDirective ManifestSrc = "manifest-src"
renderCSPDirective WorkerSrc = "worker-src"
renderCSPDirective ChildSrc = "child-src"
renderCSPDirective ReportUri = "report-uri"
renderCSPDirective ReportTo = "report-to"
renderCSPDirective UpgradeInsecureRequests = "upgrade-insecure-requests"
renderCSPDirective BlockAllMixedContent = "block-all-mixed-content"

-- | Render a CSP to its header value
renderCSP :: CSP -> Text
renderCSP csp = Text.intercalate "; " $ catMaybes
    [ renderSourceDirective "default-src" (defaultSrc csp)
    , renderSourceDirective "script-src" (scriptSrc csp)
    , renderSourceDirective "style-src" (styleSrc csp)
    , renderSourceDirective "img-src" (imgSrc csp)
    , renderSourceDirective "connect-src" (connectSrc csp)
    , renderSourceDirective "font-src" (fontSrc csp)
    , renderSourceDirective "object-src" (objectSrc csp)
    , renderSourceDirective "media-src" (mediaSrc csp)
    , renderSourceDirective "frame-src" (frameSrc csp)
    , renderSourceDirective "frame-ancestors" (frameAncestors csp)
    , renderSourceDirective "base-uri" (baseUri csp)
    , renderSourceDirective "form-action" (formAction csp)
    , renderSourceDirective "manifest-src" (manifestSrc csp)
    , renderSourceDirective "worker-src" (workerSrc csp)
    , renderSourceDirective "child-src" (childSrc csp)
    , renderUriDirective "report-uri" (reportUri csp)
    , renderUriDirective "report-to" (reportTo csp)
    , if upgradeInsecureRequests csp then Just "upgrade-insecure-requests" else Nothing
    , if blockAllMixedContent csp then Just "block-all-mixed-content" else Nothing
    ]
  where
    renderSourceDirective :: Text -> Maybe CSPSourceList -> Maybe Text
    renderSourceDirective name (Just sources) =
        Just $ name <> " " <> Text.intercalate " " (map renderCSPSource sources)
    renderSourceDirective _ Nothing = Nothing

    renderUriDirective :: Text -> Maybe Text -> Maybe Text
    renderUriDirective name (Just uri) = Just $ name <> " " <> uri
    renderUriDirective _ Nothing = Nothing

-- Helper to add headers to response
mapResponseHeaders :: (ResponseHeaders -> ResponseHeaders) -> Response -> Response
mapResponseHeaders f (ResponseFile status headers filePath part) =
    ResponseFile status (f headers) filePath part
mapResponseHeaders f (ResponseBuilder status headers builder) =
    ResponseBuilder status (f headers) builder
mapResponseHeaders f (ResponseStream status headers stream) =
    ResponseStream status (f headers) stream
mapResponseHeaders f (ResponseRaw action response) =
    ResponseRaw action (mapResponseHeaders f response)

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr go []
  where
    go Nothing xs = xs
    go (Just x) xs = x : xs
