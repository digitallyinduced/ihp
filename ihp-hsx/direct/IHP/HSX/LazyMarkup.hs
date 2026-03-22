{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, UndecidableInstances, BangPatterns #-}

{-|
Module: IHP.HSX.LazyMarkup
Description: HTML markup using lazy ByteString concatenation directly.
             No Builder abstraction — just LBS.ByteString append.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.LazyMarkup
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
import Data.String.Conversions (cs)
import Data.String (IsString(..))

-- | HTML markup type backed by lazy ByteString directly.
-- No Builder abstraction — just lazy ByteString concatenation.
newtype Markup = Markup { getBuilder :: LBS.ByteString }

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
    show m = cs (renderMarkup m)

-- | Render markup to a lazy ByteString.
renderMarkup :: Markup -> LBS.ByteString
renderMarkup (Markup b) = b
{-# INLINE renderMarkup #-}

-- | Render markup to a strict ByteString.
renderMarkupBS :: Markup -> ByteString
renderMarkupBS = LBS.toStrict . renderMarkup
{-# INLINE renderMarkupBS #-}

-- | Emit pre-encoded bytes.
rawByteString :: ByteString -> Markup
rawByteString = Markup . LBS.fromStrict
{-# NOINLINE rawByteString #-}

-- | Emit HTML-escaped text.
escapeHtml :: Text -> Markup
escapeHtml text = Markup (escapeHtmlLBS text)
{-# NOINLINE escapeHtml #-}

-- | HTML text escaper producing lazy ByteString directly.
escapeHtmlLBS :: Text -> LBS.ByteString
escapeHtmlLBS = go
  where
    go !t = case Text.break needsEscaping t of
        (safe, rest)
            | Text.null safe, Text.null rest -> mempty
            | Text.null rest -> LBS.fromStrict (TE.encodeUtf8 safe)
            | Text.null safe ->
                escapeChar (Text.head rest) <> go (Text.tail rest)
            | otherwise ->
                LBS.fromStrict (TE.encodeUtf8 safe) <> escapeChar (Text.head rest) <> go (Text.tail rest)

    needsEscaping c = c == '&' || c == '<' || c == '>' || c == '"'

    escapeChar '&' = "&amp;"
    escapeChar '<' = "&lt;"
    escapeChar '>' = "&gt;"
    escapeChar '"' = "&quot;"
    escapeChar _   = mempty
    {-# INLINE escapeChar #-}
{-# INLINE escapeHtmlLBS #-}

-- | Emit an HTML comment.
textComment :: Text -> Markup
textComment t = Markup ("<!-- " <> LBS.fromStrict (TE.encodeUtf8 t) <> " -->")
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
        Markup (LBS.fromStrict (TE.encodeUtf8 prefix) <> escapeHtmlLBS value <> "\"")

instance ApplyAttribute String where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (Text.pack value)

instance ApplyAttribute Bool where
    {-# NOINLINE applyAttribute #-}
    applyAttribute name prefix True
        | "data-" `Text.isPrefixOf` name =
            Markup (LBS.fromStrict (TE.encodeUtf8 prefix) <> "true\"")
        | otherwise =
            Markup (LBS.fromStrict (TE.encodeUtf8 prefix) <> LBS.fromStrict (TE.encodeUtf8 name) <> "\"")
    applyAttribute name prefix False
        | "data-" `Text.isPrefixOf` name =
            Markup (LBS.fromStrict (TE.encodeUtf8 prefix) <> "false\"")
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
