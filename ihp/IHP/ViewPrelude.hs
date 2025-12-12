{-# OPTIONS_HADDOCK not-home, hide #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module IHP.ViewPrelude (
    module IHP.Prelude,
    module IHP.View.TimeAgo,
    stringValue,
    module IHP.View.Form,
    module IHP.View.Types,
    hsx,
    toHtml,
    preEscapedToHtml,
    preEscapedTextValue,
    module IHP.ValidationSupport,
    pathTo,
    urlTo,
    module IHP.ViewSupport,
    module IHP.ModelSupport,
    module IHP.FrameworkConfig,
    (!),
    module Data.Data,
    module Data.Aeson,
    module IHP.AutoRefresh.View,
    module IHP.View.Classes,
    module IHP.FlashMessages,
    module IHP.Controller.Context,
    module IHP.Controller.Layout,
    module IHP.Modal.Types,
    module IHP.Modal.ViewFunctions,
    module IHP.Job.Types,
    module IHP.LoginSupport.Helper.View,
    module IHP.PageHead.ViewFunctions,
    module IHP.Breadcrumb.ViewFunctions,
    module IHP.Pagination.ViewFunctions,
) where

import IHP.Prelude
import           IHP.ViewSupport
import Text.Blaze (stringValue, (!))
import Text.Blaze.Html5 (preEscapedToHtml, preEscapedTextValue)
import IHP.View.Form
import IHP.HSX.QQ (hsx)
import IHP.HSX.ToHtml
import IHP.View.TimeAgo
import IHP.ValidationSupport
import IHP.RouterSupport
import IHP.ModelSupport
import IHP.FrameworkConfig
import Data.Data
import Data.Aeson (ToJSON (..), FromJSON (..), KeyValue (..))
import IHP.AutoRefresh.View
import IHP.View.Types
import IHP.View.Classes
import IHP.FlashMessages
import IHP.Controller.Context
import IHP.Controller.Layout

import IHP.Modal.Types
import IHP.Modal.ViewFunctions
import IHP.Job.Types

import IHP.LoginSupport.Helper.View
import IHP.PageHead.ViewFunctions

import IHP.Breadcrumb.ViewFunctions
import IHP.Pagination.ViewFunctions
