{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, UndecidableInstances, BangPatterns #-}

{-|
Module: IHP.HSX.StrictMarkup
Description: HTML markup using bytestring-strict-builder for single-allocation output.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.StrictMarkup
( Markup (..)
, renderMarkup
, renderMarkupBS
, rawByteString
, escapeHtml
, textComment
, ToHtml (..)
, ApplyAttribute (..)
, spreadAttributes
) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified ByteString.StrictBuilder as SB
import Data.String.Conversions (cs)
import Data.String (IsString(..))
import Data.Word (Word8)

-- | Efficient HTML markup type backed by bytestring-strict-builder.
-- Pre-computes output size for single-allocation rendering.
newtype Markup = Markup { getBuilder :: SB.Builder }

instance Semigroup Markup where
    {-# INLINE (<>) #-}
    Markup a <> Markup b = Markup (a <> b)

instance Monoid Markup where
    {-# INLINE mempty #-}
    mempty = Markup mempty
    {-# INLINE mconcat #-}
    mconcat = Markup . mconcat . map getBuilder

instance IsString Markup where
    {-# INLINE fromString #-}
    fromString = escapeHtml . Text.pack

instance Show Markup where
    show m = cs (renderMarkupBS m)

-- | Render markup to a lazy ByteString (for WAI compatibility).
renderMarkup :: Markup -> LBS.ByteString
renderMarkup (Markup b) = LBS.fromStrict (SB.builderBytes b)
{-# INLINE renderMarkup #-}

-- | Render markup to a strict ByteString.
renderMarkupBS :: Markup -> ByteString
renderMarkupBS (Markup b) = SB.builderBytes b
{-# INLINE renderMarkupBS #-}

-- | Emit pre-encoded bytes. Used for compile-time static HTML.
rawByteString :: ByteString -> Markup
rawByteString = Markup . SB.bytes
{-# INLINE rawByteString #-}

-- | Emit HTML-escaped text.
-- Scans for special characters in chunks, encoding safe runs in bulk.
escapeHtml :: Text -> Markup
escapeHtml text = Markup (escapeHtmlBuilder text)
{-# INLINE escapeHtml #-}

-- | Efficient HTML text escaper using bytestring-strict-builder.
-- Processes safe runs in bulk via encodeUtf8, only breaking for special characters.
escapeHtmlBuilder :: Text -> SB.Builder
escapeHtmlBuilder = go
  where
    go !t = case Text.break needsEscaping t of
        (safe, rest)
            | Text.null safe, Text.null rest -> mempty
            | Text.null rest -> SB.bytes (TE.encodeUtf8 safe)
            | Text.null safe ->
                escapeChar (Text.head rest) <> go (Text.tail rest)
            | otherwise ->
                SB.bytes (TE.encodeUtf8 safe) <> escapeChar (Text.head rest) <> go (Text.tail rest)

    needsEscaping c = c == '&' || c == '<' || c == '>' || c == '"'

    escapeChar '&' = SB.bytes "&amp;"
    escapeChar '<' = SB.bytes "&lt;"
    escapeChar '>' = SB.bytes "&gt;"
    escapeChar '"' = SB.bytes "&quot;"
    escapeChar c   = SB.utf8Char c
    {-# INLINE escapeChar #-}
{-# INLINE escapeHtmlBuilder #-}

-- | Emit an HTML comment.
textComment :: Text -> Markup
textComment t = Markup (SB.bytes "<!-- " <> SB.bytes (TE.encodeUtf8 t) <> SB.bytes " -->")
{-# INLINE textComment #-}

-- | Convert a value to HTML markup.
class ToHtml a where
    toHtml :: a -> Markup

instance ToHtml Markup where
    {-# INLINE toHtml #-}
    toHtml = id

instance ToHtml Text where
    {-# INLINE toHtml #-}
    toHtml = escapeHtml

instance ToHtml String where
    {-# INLINE toHtml #-}
    toHtml = escapeHtml . Text.pack

instance ToHtml ByteString where
    {-# INLINE toHtml #-}
    toHtml = toHtml . (cs :: ByteString -> Text)

instance {-# OVERLAPPABLE #-} ToHtml a => ToHtml (Maybe a) where
    {-# INLINE toHtml #-}
    toHtml = maybe mempty toHtml

instance {-# OVERLAPPABLE #-} Show a => ToHtml a where
    {-# INLINE toHtml #-}
    toHtml = toHtml . show

-- | Apply a dynamic attribute value.
class ApplyAttribute value where
    applyAttribute :: Text -> Text -> value -> Markup

instance ApplyAttribute Text where
    {-# INLINE applyAttribute #-}
    applyAttribute _name prefix value =
        Markup (SB.bytes (TE.encodeUtf8 prefix) <> escapeHtmlBuilder value <> SB.word8 0x22)

instance ApplyAttribute String where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (Text.pack value)

instance ApplyAttribute Bool where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix True
        | "data-" `Text.isPrefixOf` name =
            Markup (SB.bytes (TE.encodeUtf8 prefix) <> SB.bytes "true\"")
        | otherwise =
            Markup (SB.bytes (TE.encodeUtf8 prefix) <> SB.bytes (TE.encodeUtf8 name) <> SB.word8 0x22)
    applyAttribute name prefix False
        | "data-" `Text.isPrefixOf` name =
            Markup (SB.bytes (TE.encodeUtf8 prefix) <> SB.bytes "false\"")
        | otherwise = mempty

instance ApplyAttribute a => ApplyAttribute (Maybe a) where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix (Just value) = applyAttribute name prefix value
    applyAttribute _name _prefix Nothing = mempty

instance {-# OVERLAPPABLE #-} Show a => ApplyAttribute a where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (show value)

-- | Apply spread attributes.
spreadAttributes :: ApplyAttribute value => [(Text, value)] -> Markup
spreadAttributes = foldMap (\(name, value) -> applyAttribute name (" " <> name <> "=\"") value)
{-# INLINE spreadAttributes #-}
