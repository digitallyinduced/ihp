{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, UndecidableInstances, BangPatterns #-}

{-|
Module: IHP.HSX.TextMarkup
Description: HTML markup using Data.Text.Lazy.Builder for benchmarking comparison.
             Builds Text via lazy Text builder, then encodes to UTF-8 ByteString.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.TextMarkup
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
import qualified Data.Text.Lazy.Builder as TLB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy.Encoding as TLE
import Data.String.Conversions (cs)
import Data.String (IsString(..))

-- | HTML markup type backed by Data.Text.Lazy.Builder.
-- Builds lazy Text, then encodes to UTF-8 for output.
newtype Markup = Markup { getBuilder :: TLB.Builder }

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

-- | Render markup to a lazy ByteString (via Text builder -> lazy Text -> UTF-8).
renderMarkup :: Markup -> LBS.ByteString
renderMarkup (Markup b) = TLE.encodeUtf8 (TLB.toLazyText b)
{-# INLINE renderMarkup #-}

-- | Render markup to a strict ByteString.
renderMarkupBS :: Markup -> ByteString
renderMarkupBS = LBS.toStrict . renderMarkup
{-# INLINE renderMarkupBS #-}

-- | Emit pre-encoded bytes as Text. Decodes the ByteString to Text first.
rawByteString :: ByteString -> Markup
rawByteString = Markup . TLB.fromText . TE.decodeUtf8
{-# NOINLINE rawByteString #-}

-- | Emit HTML-escaped text.
-- Scans for special characters in chunks, encoding safe runs in bulk.
escapeHtml :: Text -> Markup
escapeHtml text = Markup (escapeHtmlBuilder text)
{-# NOINLINE escapeHtml #-}

-- | Efficient HTML text escaper using Text.Lazy.Builder.
-- Processes safe runs in bulk, only breaking for special characters.
escapeHtmlBuilder :: Text -> TLB.Builder
escapeHtmlBuilder = go
  where
    go !t = case Text.break needsEscaping t of
        (safe, rest)
            | Text.null safe, Text.null rest -> mempty
            | Text.null rest -> TLB.fromText safe
            | Text.null safe ->
                escapeChar (Text.head rest) <> go (Text.tail rest)
            | otherwise ->
                TLB.fromText safe <> escapeChar (Text.head rest) <> go (Text.tail rest)

    needsEscaping c = c == '&' || c == '<' || c == '>' || c == '"'

    escapeChar '&' = TLB.fromText "&amp;"
    escapeChar '<' = TLB.fromText "&lt;"
    escapeChar '>' = TLB.fromText "&gt;"
    escapeChar '"' = TLB.fromText "&quot;"
    escapeChar c   = TLB.singleton c
    {-# INLINE escapeChar #-}
{-# INLINE escapeHtmlBuilder #-}

-- | Emit an HTML comment.
textComment :: Text -> Markup
textComment t = Markup (TLB.fromText "<!-- " <> TLB.fromText t <> TLB.fromText " -->")
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
        Markup (TLB.fromText prefix <> escapeHtmlBuilder value <> TLB.singleton '"')

instance ApplyAttribute String where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (Text.pack value)

instance ApplyAttribute Bool where
    {-# NOINLINE applyAttribute #-}
    applyAttribute name prefix True
        | "data-" `Text.isPrefixOf` name =
            Markup (TLB.fromText prefix <> TLB.fromText "true\"")
        | otherwise =
            Markup (TLB.fromText prefix <> TLB.fromText name <> TLB.singleton '"')
    applyAttribute name prefix False
        | "data-" `Text.isPrefixOf` name =
            Markup (TLB.fromText prefix <> TLB.fromText "false\"")
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
