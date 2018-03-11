{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Foundation.View.ConvertibleStrings where
import           ClassyPrelude
import           Data.String.Conversions (ConvertibleStrings (convertString), cs)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5        as Html5

instance ConvertibleStrings String Html5.AttributeValue where
    convertString = stringValue

instance ConvertibleStrings Text Html5.AttributeValue where
    convertString = Html5.textValue

instance ConvertibleStrings String Html5.Html where
    convertString = Html5.string

instance ConvertibleStrings Text Html5.Html where
    convertString = Html5.text

