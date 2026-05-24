{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-|
Module: IHP.Router.Capture
Description: Type class for parsing and rendering URL path segments

'UrlCapture' is used by the explicit-routes layer (see "IHP.Router.Trie" and
"IHP.Router.DSL") to convert between URL path segments and typed Haskell
values. Each instance specifies how a single segment is decoded into a
value and re-encoded back into URL-safe text.

The base instances ('Text', 'Int', 'Integer', 'UUID', 'Bool', 'Day',
'Segment') live here in @ihp-router@ and have no IHP dependency. IHP's
@'IHP.ModelSupport.Id''@ orphan instance lives in "IHP.Router.IHP" so
plain WAI users of @ihp-router@ aren't dragged into IHP's model layer.
-}
module IHP.Router.Capture
    ( UrlCapture (..)
    , Segment (..)
    ) where

import Prelude
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Typeable (Typeable)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Data.Time.Calendar (Day)
import qualified Data.Time.Format as Time
import Text.Read (readMaybe)

-- | A type that can appear as a URL path segment.
--
-- Instances provide a 'parseCapture' for decoding a raw segment bytestring
-- (post-URL-decoding) into a typed value, and 'renderCapture' for the
-- reverse direction when generating URLs via 'IHP.Router.UrlGenerator.pathTo'.
--
-- __Example:__
--
-- >>> parseCapture @Int "42"
-- Just 42
--
-- >>> renderCapture (42 :: Int)
-- "42"
class Typeable a => UrlCapture a where
    -- | Parse a single URL segment (already URL-decoded) into a typed value.
    -- Returns 'Nothing' if the segment cannot be interpreted as this type.
    parseCapture :: ByteString -> Maybe a

    -- | Render a typed value as URL-ready text.
    -- The caller is responsible for URL-encoding if needed.
    renderCapture :: a -> Text

instance UrlCapture Text where
    parseCapture bs = case Text.decodeUtf8' bs of
        Right t -> Just t
        Left _  -> Nothing
    {-# INLINE parseCapture #-}
    renderCapture = id
    {-# INLINE renderCapture #-}

instance UrlCapture Int where
    parseCapture = readMaybe . ByteString.Char8.unpack
    {-# INLINE parseCapture #-}
    renderCapture = Text.pack . show
    {-# INLINE renderCapture #-}

instance UrlCapture Integer where
    parseCapture = readMaybe . ByteString.Char8.unpack
    {-# INLINE parseCapture #-}
    renderCapture = Text.pack . show
    {-# INLINE renderCapture #-}

instance UrlCapture UUID where
    parseCapture = UUID.fromASCIIBytes
    {-# INLINE parseCapture #-}
    renderCapture = UUID.toText
    {-# INLINE renderCapture #-}

instance UrlCapture Bool where
    parseCapture bs = case bs of
        "true"  -> Just True
        "false" -> Just False
        "1"     -> Just True
        "0"     -> Just False
        _       -> Nothing
    {-# INLINE parseCapture #-}
    renderCapture True  = "true"
    renderCapture False = "false"
    {-# INLINE renderCapture #-}

instance UrlCapture Day where
    parseCapture = Time.parseTimeM True Time.defaultTimeLocale "%Y-%m-%d" . ByteString.Char8.unpack
    {-# INLINE parseCapture #-}
    renderCapture = Text.pack . Time.formatTime Time.defaultTimeLocale "%Y-%m-%d"
    {-# INLINE renderCapture #-}

-- | A URL path segment guaranteed to be non-empty.
--
-- Useful when a capture must not match an empty string. Plain 'Text' captures
-- happily match @""@ (the segment between two consecutive slashes); 'Segment'
-- rejects that case, which is often what you want for splat captures or
-- required path pieces.
newtype Segment = Segment { unSegment :: Text }
    deriving (Eq, Ord, Show)

instance UrlCapture Segment where
    parseCapture bs
        | ByteString.null bs = Nothing
        | otherwise = Segment <$> parseCapture @Text bs
    {-# INLINE parseCapture #-}
    renderCapture (Segment t) = t
    {-# INLINE renderCapture #-}
