{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, UndecidableInstances, BangPatterns #-}

{-|
Module: IHP.HSX.ShortMarkup
Description: HTML markup using ShortByteString DList for benchmarking comparison.
             Accumulates ShortByteString fragments, then concat for single-alloc output.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.ShortMarkup
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
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as SBS
import Data.String.Conversions (cs)
import Data.String (IsString(..))
import Data.Monoid (Endo(..))

-- | HTML markup type backed by a DList of ShortByteStrings.
-- Accumulates unpinned byte fragments, then SBS.concat for single-alloc output.
newtype Markup = Markup { getBuilder :: Endo [ShortByteString] }

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
renderMarkup m = LBS.fromStrict (renderMarkupBS m)
{-# INLINE renderMarkup #-}

-- | Render markup to a strict ByteString.
renderMarkupBS :: Markup -> ByteString
renderMarkupBS (Markup b) = SBS.fromShort (SBS.concat (appEndo b []))
{-# INLINE renderMarkupBS #-}

-- | Emit a single ShortByteString fragment.
emitShort :: ShortByteString -> Markup
emitShort sbs = Markup (Endo (sbs :))
{-# INLINE emitShort #-}

-- | Emit pre-encoded bytes. Used for compile-time static HTML.
rawByteString :: ByteString -> Markup
rawByteString = emitShort . SBS.toShort
{-# NOINLINE rawByteString #-}

-- | Emit HTML-escaped text.
escapeHtml :: Text -> Markup
escapeHtml text = escapeHtmlMarkup text
{-# NOINLINE escapeHtml #-}

-- | Efficient HTML text escaper producing ShortByteString fragments.
-- Processes safe runs in bulk, only breaking for special characters.
escapeHtmlMarkup :: Text -> Markup
escapeHtmlMarkup = go
  where
    go !t = case Text.break needsEscaping t of
        (safe, rest)
            | Text.null safe, Text.null rest -> mempty
            | Text.null rest -> emitShort (SBS.toShort (TE.encodeUtf8 safe))
            | Text.null safe ->
                escapeChar (Text.head rest) <> go (Text.tail rest)
            | otherwise ->
                emitShort (SBS.toShort (TE.encodeUtf8 safe)) <> escapeChar (Text.head rest) <> go (Text.tail rest)

    needsEscaping c = c == '&' || c == '<' || c == '>' || c == '"'

    escapeChar '&' = emitShort "&amp;"
    escapeChar '<' = emitShort "&lt;"
    escapeChar '>' = emitShort "&gt;"
    escapeChar '"' = emitShort "&quot;"
    escapeChar _   = mempty
    {-# INLINE escapeChar #-}
{-# INLINE escapeHtmlMarkup #-}

-- | Emit an HTML comment.
textComment :: Text -> Markup
textComment t = emitShort "<!-- " <> emitShort (SBS.toShort (TE.encodeUtf8 t)) <> emitShort " -->"
{-# NOINLINE textComment #-}

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
    {-# NOINLINE applyAttribute #-}
    applyAttribute _name prefix value =
        emitShort (SBS.toShort (TE.encodeUtf8 prefix)) <> escapeHtmlMarkup value <> emitShort "\""

instance ApplyAttribute String where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (Text.pack value)

instance ApplyAttribute Bool where
    {-# NOINLINE applyAttribute #-}
    applyAttribute name prefix True
        | "data-" `Text.isPrefixOf` name =
            emitShort (SBS.toShort (TE.encodeUtf8 prefix)) <> emitShort "true\""
        | otherwise =
            emitShort (SBS.toShort (TE.encodeUtf8 prefix)) <> emitShort (SBS.toShort (TE.encodeUtf8 name)) <> emitShort "\""
    applyAttribute name prefix False
        | "data-" `Text.isPrefixOf` name =
            emitShort (SBS.toShort (TE.encodeUtf8 prefix)) <> emitShort "false\""
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
