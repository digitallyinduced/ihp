{-|
Module: IHP.PageHead.Types
Copyright: (c) digitally induced GmbH, 2021
-}
module IHP.PageHead.Types where

import IHP.Prelude

newtype PageTitle = PageTitle Text

newtype OGTitle = OGTitle Text

newtype OGType = OGType Text

newtype OGDescription = OGDescription Text

newtype OGUrl = OGUrl Text

newtype OGImage = OGImage Text 