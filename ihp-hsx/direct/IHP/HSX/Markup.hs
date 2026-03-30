{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances, UndecidableInstances, BangPatterns #-}

{-|
Module: IHP.HSX.Markup
Description: High-performance HTML markup type using ByteString Builder directly.
             Eliminates the intermediate tree structure used by Blaze.
Copyright: (c) digitally induced GmbH, 2024
-}
module IHP.HSX.Markup
( MarkupM (..)
, Markup
, Html
, renderMarkup
, renderMarkupBS
, renderMarkupText
, renderMarkupLazyText
, rawByteString
, escapeHtml
, textComment
, ToHtml (..)
, ApplyAttribute (..)
, AttributeValue (..)
, spreadAttributes
-- * Blaze compatibility
, preEscapedToHtml
, preEscapedTextValue
, stringValue
) where

import Prelude
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LTE
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Builder.Extra as Extra
import qualified Data.ByteString.Builder.Prim as BP
import Data.String.Conversions (ConvertibleStrings, cs)
import Data.String (IsString(..))
import Data.Word (Word8)
import Unsafe.Coerce (unsafeCoerce)

-- | Efficient HTML markup type backed by ByteString Builder.
-- Unlike Blaze's MarkupM, this does not build an intermediate tree.
-- All operations directly append to the builder.
-- The phantom type parameter mirrors Blaze's MarkupM for compatibility
-- with 'forEach' and other Applicative/Monad-polymorphic code.
newtype MarkupM a = Markup { getBuilder :: Builder }

-- | The standard markup type, equivalent to Blaze's @Html@.
type Markup = MarkupM ()

-- | Alias for 'Markup', matching Blaze's @Html@ so existing user code
-- (e.g. @defaultLayout :: Html -> Html@) keeps compiling without changes.
type Html = Markup

instance Semigroup (MarkupM a) where
    Markup a <> Markup b = Markup (a <> b)

instance Monoid (MarkupM a) where
    {-# INLINE mempty #-}
    mempty = Markup mempty
    {-# INLINE mconcat #-}
    mconcat = Markup . mconcat . map getBuilder

instance Functor MarkupM where
    {-# INLINE fmap #-}
    fmap _ (Markup b) = Markup b

instance Applicative MarkupM where
    {-# INLINE pure #-}
    pure _ = Markup mempty
    {-# INLINE (<*>) #-}
    Markup a <*> Markup b = Markup (a <> b)

instance Monad MarkupM where
    {-# INLINE (>>=) #-}
    -- The phantom value is never inspected (forM_ uses \_ -> ...).
    -- unsafeCoerce avoids allocating an error thunk.
    Markup a >>= f = let Markup b = f (unsafeCoerce ()) in Markup (a <> b)
    {-# INLINE (>>) #-}
    Markup a >> Markup b = Markup (a <> b)

instance IsString Markup where
    {-# INLINE fromString #-}
    fromString = escapeHtml . Text.pack

instance Show Markup where
    show m = Text.unpack (renderMarkupText m)

-- | Render markup to a lazy ByteString.
-- Uses 32KB untrimmed buffers: fewer, larger chunks means less overhead for
-- warp's response sending and for 'evaluate . length' in respondHtml.
renderMarkup :: Markup -> LBS.ByteString
renderMarkup (Markup b) = Extra.toLazyByteStringWith
    (Extra.untrimmedStrategy 32768 32768) LBS.empty b
{-# INLINE renderMarkup #-}

-- | Render markup to a strict ByteString.
-- Uses 32KB untrimmed first buffer so typical HTML pages (< 32KB) fit in a
-- single chunk, making the toStrict conversion zero-copy.
renderMarkupBS :: Markup -> ByteString
renderMarkupBS (Markup b) = LBS.toStrict
    $ Extra.toLazyByteStringWith (Extra.untrimmedStrategy 32768 32768) LBS.empty b
{-# INLINE renderMarkupBS #-}

-- | Render markup to a strict Text.
renderMarkupText :: Markup -> Text
renderMarkupText = TE.decodeUtf8 . renderMarkupBS
{-# INLINE renderMarkupText #-}

-- | Render markup to a lazy Text.
renderMarkupLazyText :: Markup -> LT.Text
renderMarkupLazyText = LTE.decodeUtf8 . renderMarkup
{-# INLINE renderMarkupLazyText #-}

-- | Emit pre-encoded bytes. Used for compile-time static HTML.
rawByteString :: ByteString -> Markup
rawByteString = Markup . Builder.byteString
{-# NOINLINE rawByteString #-}

-- | Emit HTML-escaped text. NOINLINE to avoid duplicating the BoundedPrim
-- escaping logic at every call site (each condB chain is ~70 Core terms).
escapeHtml :: Text -> Markup
escapeHtml = Markup . TE.encodeUtf8BuilderEscaped htmlEscapedW8
{-# NOINLINE escapeHtml #-}

-- | BoundedPrim that escapes HTML special characters: & < > "
-- Matches Blaze's escaping behavior.
-- Bytes > 0x7e are passed through (multi-byte UTF-8 lead bytes, never ASCII specials).
htmlEscapedW8 :: BP.BoundedPrim Word8
htmlEscapedW8 =
    BP.condB (>  0x7e) (BP.liftFixedToBounded BP.word8) $
    BP.condB (== 0x26) (fixed5 (0x26, (0x61, (0x6d, (0x70, 0x3b))))) $         -- & → &amp;
    BP.condB (== 0x3c) (fixed4 (0x26, (0x6c, (0x74, 0x3b)))) $                 -- < → &lt;
    BP.condB (== 0x3e) (fixed4 (0x26, (0x67, (0x74, 0x3b)))) $                 -- > → &gt;
    BP.condB (== 0x22) (fixed6 (0x26, (0x71, (0x75, (0x6f, (0x74, 0x3b)))))) $ -- " → &quot;
    BP.liftFixedToBounded BP.word8
  where
    {-# INLINE fixed4 #-}
    fixed4 x = BP.liftFixedToBounded $ const x BP.>$< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8
    {-# INLINE fixed5 #-}
    fixed5 x = BP.liftFixedToBounded $ const x BP.>$< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8
    {-# INLINE fixed6 #-}
    fixed6 x = BP.liftFixedToBounded $ const x BP.>$< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8 BP.>*< BP.word8
{-# INLINE htmlEscapedW8 #-}

-- | Emit an HTML comment.
textComment :: Text -> Markup
textComment t = Markup (Builder.byteString "<!-- " <> TE.encodeUtf8Builder t <> Builder.byteString " -->")
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

-- | Apply a dynamic attribute value. Returns the attribute fragment as Markup.
--
-- First argument: attribute name (e.g. "class")
-- Second argument: pre-computed prefix (e.g. " class=\"")
-- The result includes the closing quote.
class ApplyAttribute value where
    applyAttribute :: Text -> Text -> value -> Markup

instance ApplyAttribute Text where
    {-# NOINLINE applyAttribute #-}
    applyAttribute _name prefix value =
        Markup (TE.encodeUtf8Builder prefix <> TE.encodeUtf8BuilderEscaped htmlEscapedW8 value <> Builder.char8 '"')

instance ApplyAttribute String where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (Text.pack value)

instance ApplyAttribute ByteString where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix value = applyAttribute name prefix (cs value :: Text)

instance ApplyAttribute Bool where
    {-# NOINLINE applyAttribute #-}
    applyAttribute name prefix True
        | "data-" `Text.isPrefixOf` name =
            Markup (TE.encodeUtf8Builder prefix <> Builder.byteString "true\"")
        | otherwise =
            Markup (TE.encodeUtf8Builder prefix <> TE.encodeUtf8Builder name <> Builder.char8 '"')
    applyAttribute name prefix False
        | "data-" `Text.isPrefixOf` name =
            Markup (TE.encodeUtf8Builder prefix <> Builder.byteString "false\"")
        | otherwise = mempty

instance ApplyAttribute Markup where
    {-# INLINE applyAttribute #-}
    applyAttribute _name prefix (Markup b) =
        Markup (TE.encodeUtf8Builder prefix <> b <> Builder.char8 '"')

instance ApplyAttribute a => ApplyAttribute (Maybe a) where
    {-# INLINE applyAttribute #-}
    applyAttribute name prefix (Just value) = applyAttribute name prefix value
    applyAttribute _name _prefix Nothing = mempty

-- | Converts a value to a 'Builder' for use as an HTML attribute value.
-- Returns an HTML-escaped ByteString Builder so the result can be spliced
-- directly into the markup without an intermediate Text allocation.
class AttributeValue a where
    attributeValue :: a -> Builder

instance AttributeValue Text where
    {-# INLINE attributeValue #-}
    attributeValue = TE.encodeUtf8BuilderEscaped htmlEscapedW8

instance AttributeValue String where
    {-# INLINE attributeValue #-}
    attributeValue = TE.encodeUtf8BuilderEscaped htmlEscapedW8 . Text.pack

instance AttributeValue Int where
    {-# INLINE attributeValue #-}
    attributeValue = Builder.intDec

instance AttributeValue Integer where
    {-# INLINE attributeValue #-}
    attributeValue = Builder.integerDec

instance AttributeValue Double where
    {-# INLINE attributeValue #-}
    attributeValue = Builder.string8 . show

instance AttributeValue Float where
    {-# INLINE attributeValue #-}
    attributeValue = Builder.string8 . show

instance {-# OVERLAPPABLE #-} AttributeValue a => ApplyAttribute a where
    {-# INLINE applyAttribute #-}
    applyAttribute _name prefix value =
        Markup (TE.encodeUtf8Builder prefix <> attributeValue value <> Builder.char8 '"')

-- | Apply spread attributes.
spreadAttributes :: ApplyAttribute value => [(Text, value)] -> Markup
spreadAttributes = foldMap (\(name, value) -> applyAttribute name (" " <> name <> "=\"") value)
{-# INLINE spreadAttributes #-}

-- | Blaze compatibility: emit pre-escaped HTML (no escaping applied).
-- Use for trusted HTML content only. Accepts Text, String, ByteString, etc.
preEscapedToHtml :: ConvertibleStrings s Text => s -> Markup
preEscapedToHtml = Markup . TE.encodeUtf8Builder . cs
{-# INLINE preEscapedToHtml #-}

-- | Blaze compatibility: pre-escaped text value for attributes.
-- Returns Markup containing the raw text (no escaping).
preEscapedTextValue :: Text -> Markup
preEscapedTextValue = Markup . TE.encodeUtf8Builder
{-# INLINE preEscapedTextValue #-}

-- | Blaze compatibility: convert a String to a Markup value (HTML-escaped).
stringValue :: String -> Markup
stringValue = escapeHtml . Text.pack
{-# INLINE stringValue #-}
