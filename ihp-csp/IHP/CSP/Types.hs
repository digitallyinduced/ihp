{-|
Module: IHP.CSP.Types
Description: Type-safe Content Security Policy (CSP) types
Copyright: (c) digitally induced GmbH, 2025

This module provides type-safe representations for Content Security Policy directives.
-}
module IHP.CSP.Types
    ( CSP (..)
    , CSPDirective (..)
    , CSPSource (..)
    , CSPSourceList
    , defaultCSP
    , strictCSP
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
    , renderCSP
    , renderCSPDirective
    , renderCSPSource
    ) where

import ClassyPrelude
import qualified Data.Text as Text
import Data.String (IsString(..))

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
-- This provides strong XSS protection
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

-- | 'self' source
self :: CSPSource
self = Self

-- | 'unsafe-inline' source
unsafeInline :: CSPSource
unsafeInline = UnsafeInline

-- | 'unsafe-eval' source
unsafeEval :: CSPSource
unsafeEval = UnsafeEval

-- | 'strict-dynamic' source
strictDynamic :: CSPSource
strictDynamic = StrictDynamic

-- | 'none' source
none :: CSPSource
none = None

-- | 'data:' source
data' :: CSPSource
data' = Data

-- | 'https:' source
https :: CSPSource
https = Https

-- | 'http:' source
http :: CSPSource
http = Http

-- | 'blob:' source
blob :: CSPSource
blob = Blob

-- | 'mediastream:' source
mediastream :: CSPSource
mediastream = Mediastream

-- | 'filesystem:' source
filesystem :: CSPSource
filesystem = Filesystem

-- | Create a nonce source
nonce :: Text -> CSPSource
nonce = Nonce

-- | Create a host source (e.g., "example.com", "*.example.com")
host :: Text -> CSPSource
host = Host

-- | Create a scheme source (e.g., "https:", "data:")
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
