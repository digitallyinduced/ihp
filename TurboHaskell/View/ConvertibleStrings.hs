{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module TurboHaskell.View.ConvertibleStrings where
import           ClassyPrelude
import           Data.String.Conversions (ConvertibleStrings (convertString), cs)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5        as Html5
import qualified Data.ByteString.Lazy as LBS
import TurboHaskell.RouterSupport (HasPath (..))

instance ConvertibleStrings String Html5.AttributeValue where
    {-# INLINE convertString #-}
    convertString = stringValue

instance ConvertibleStrings Text Html5.AttributeValue where
    {-# INLINE convertString #-}
    convertString = Html5.textValue

instance ConvertibleStrings String Html5.Html where
    {-# INLINE convertString #-}
    convertString = Html5.string

instance ConvertibleStrings ByteString Html5.AttributeValue where
    {-# INLINE convertString #-}
    convertString value = convertString (cs value :: Text)

instance ConvertibleStrings LBS.ByteString Html5.AttributeValue where
    {-# INLINE convertString #-}
    convertString value = convertString (cs value :: Text)

instance ConvertibleStrings Text Html5.Html where
    {-# INLINE convertString #-}
    convertString = Html5.text

instance {-# OVERLAPPABLE #-} HasPath action => ConvertibleStrings action AttributeValue where
    convertString action = textValue (pathTo action)