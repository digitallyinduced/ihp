{-|
Module: IHP.CSP
Description: Type-safe Content Security Policy (CSP) for IHP
Copyright: (c) digitally induced GmbH, 2025

This module provides a type-safe API for managing Content Security Policy (CSP) headers in IHP applications.

CSP is an important security feature that helps prevent XSS attacks, clickjacking, and other code injection attacks.

= Quick Start

To use CSP in your IHP application:

1. Generate a nonce and store it in the controller context
2. Set the CSP header with the nonce
3. Use the nonce in your views for inline scripts and styles

Example:

@
module Web.Controller.Posts where

import Web.Controller.Prelude
import qualified IHP.CSP as CSP

instance Controller PostsController where
    beforeAction = do
        -- Generate a nonce and store it in context
        nonce <- CSP.generateNonce
        putContext (CSP.CSPNonce nonce)
        
        -- Set a strict CSP with the nonce
        CSP.setCSP $ CSP.strictCSP nonce

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
@

In your view:

@
scripts :: Html
scripts = do
    let CSP.CSPNonce nonce = fromFrozenContext
    [hsx|
        \<script nonce={nonce}\>
            console.log("This script is allowed by CSP");
        \</script\>
    |]
@

= Custom CSP Policies

You can customize the CSP policy:

@
import qualified IHP.CSP as CSP

myCustomCSP :: Text -> CSP.CSP
myCustomCSP nonce = CSP.defaultCSP
    { CSP.scriptSrc = Just [CSP.nonce nonce, CSP.strictDynamic]
    , CSP.styleSrc = Just [CSP.nonce nonce, CSP.self]
    , CSP.imgSrc = Just [CSP.self, CSP.data', CSP.host "https://cdn.example.com"]
    , CSP.connectSrc = Just [CSP.self, CSP.host "wss://example.com"]
    , CSP.frameAncestors = Just [CSP.host "https://trusted.com"]
    }
@
-}
module IHP.CSP
    ( -- * Setting CSP Headers
      setCSP
    , setCSPHeader
    
    -- * Nonce Generation
    , generateNonce
    , CSPNonce(..)
    
    -- * CSP Types
    , CSP(..)
    , CSPDirective(..)
    , CSPSource(..)
    , CSPSourceList
    
    -- * Predefined Policies
    , defaultCSP
    , strictCSP
    
    -- * CSP Source Helpers
    , nonce
    , self
    , unsafeInline
    , unsafeEval
    , strictDynamic
    , none
    , data'
    , https
    , http
    , blob
    , mediastream
    , filesystem
    , host
    , scheme
    
    -- * Rendering (for advanced use)
    , renderCSP
    , renderCSPDirective
    , renderCSPSource
    ) where

import IHP.CSP.Types
import IHP.CSP.ControllerFunctions
