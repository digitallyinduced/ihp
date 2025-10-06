{-# LANGUAGE AllowAmbiguousTypes #-}
{-|
Module: IHP.HSX.Lucid2.ToHtml
Description: Provides an alternative ToHtml class for Lucid2 that tracks the wrapped monad
Copyright: (c) digitally induced GmbH, 2025
-}

module IHP.HSX.Lucid2.ToHtml where

import Prelude (Applicative (..), Monad (..), String, ($), (.), id, mapM_)
import Lucid.Base (HtmlT (..))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL (ByteString)
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL (Text)
import qualified Lucid.Base as Lucid2 (ToHtml (..), Attributes, makeElementNoEnd, makeElement)

class (Monad m) => HtmlGen m where
    data HtmlType m :: Type

    unHtmlType :: HtmlType m -> m ()
    makeElement :: Text -> [Lucid2.Attributes] -> HtmlType m -> HtmlType m
    makeElementNoEnd :: Text -> [Lucid2.Attributes] -> HtmlType m
    sequenceChildren :: [HtmlType m] -> HtmlType m

class (HtmlGen m) => EmbedAsHtml m a where
    toHtml :: a -> HtmlType m
    toHtmlRaw :: a -> HtmlType m

instance (Monad m) => HtmlGen (HtmlT m) where
    newtype HtmlType (HtmlT m) = Lucid2Html { unLucid2Html :: HtmlT m () }
    makeElement name attrs Lucid2Html { unLucid2Html = content } =
      Lucid2Html (Lucid2.makeElement name attrs content)
    makeElementNoEnd name attrs = Lucid2Html (Lucid2.makeElementNoEnd name attrs)
    sequenceChildren = Lucid2Html . mapM_ unLucid2Html
    unHtmlType = unLucid2Html

instance (HtmlGen m) => EmbedAsHtml m (HtmlType m) where
    toHtml = id
    toHtmlRaw = id

instance (Monad m) => EmbedAsHtml (HtmlT m) String where
    toHtml = Lucid2Html . Lucid2.toHtml
    toHtmlRaw = Lucid2Html . Lucid2.toHtmlRaw

instance (Monad m) => EmbedAsHtml (HtmlT m) Text where
    toHtml = Lucid2Html . Lucid2.toHtml
    toHtmlRaw = Lucid2Html . Lucid2.toHtmlRaw

instance (Monad m) => EmbedAsHtml (HtmlT m) TL.Text where
    toHtml = Lucid2Html . Lucid2.toHtml
    toHtmlRaw = Lucid2Html . Lucid2.toHtmlRaw

instance (Monad m) => EmbedAsHtml (HtmlT m) ByteString where
    toHtml = Lucid2Html . Lucid2.toHtml
    toHtmlRaw = Lucid2Html . Lucid2.toHtmlRaw

instance (Monad m) => EmbedAsHtml (HtmlT m) BL.ByteString where
    toHtml = Lucid2Html . Lucid2.toHtml
    toHtmlRaw = Lucid2Html . Lucid2.toHtmlRaw

instance Lucid2.ToHtml () where
    toHtml = pure
    toHtmlRaw = pure

instance (Monad m, Lucid2.ToHtml a) => EmbedAsHtml (HtmlT m) (HtmlT m a) where
    toHtml mHtml = Lucid2Html $ mHtml >>= Lucid2.toHtml
    toHtmlRaw mHtml = Lucid2Html $ mHtml >>= Lucid2.toHtmlRaw

instance (Monad m, Lucid2.ToHtml a) => EmbedAsHtml (HtmlT m) (m a) where
    toHtml mHtml = Lucid2Html $ lift mHtml >>= Lucid2.toHtml
    toHtmlRaw mHtml = Lucid2Html $ lift mHtml >>= Lucid2.toHtmlRaw
