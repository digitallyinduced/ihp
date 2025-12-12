{-|
Module: IHP.CSP.ControllerFunctions
Description: Helper functions for working with CSP in IHP controllers
Copyright: (c) digitally induced GmbH, 2025
-}
module IHP.CSP.ControllerFunctions
    ( setCSP
    , setCSPHeader
    , generateNonce
    , CSPNonce(..)
    ) where

import ClassyPrelude
import IHP.CSP.Types
import IHP.ControllerSupport (setHeader)
import IHP.Controller.Context
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text.Encoding as Text
import qualified IHP.AuthSupport.Authentication as Auth

-- | A newtype wrapper for CSP nonces stored in the controller context
newtype CSPNonce = CSPNonce Text
    deriving (Show, Eq)

-- | Generate a cryptographically secure nonce for CSP
-- Uses the same secure random generation as IHP's authentication tokens
generateNonce :: IO Text
generateNonce = Auth.generateAuthenticationToken

-- | Set the Content-Security-Policy header using a CSP value
-- This is the recommended way to set CSP in IHP controllers
--
-- Example:
-- @
--   action MyAction = do
--       nonce <- generateNonce
--       putContext (CSPNonce nonce)
--       setCSP $ strictCSP nonce
--       render MyView { .. }
-- @
setCSP :: (?context :: ControllerContext) => CSP -> IO ()
setCSP csp = setCSPHeader (renderCSP csp)

-- | Set the Content-Security-Policy header directly with a text value
-- This is a lower-level function; prefer 'setCSP' for type safety
setCSPHeader :: (?context :: ControllerContext) => Text -> IO ()
setCSPHeader cspValue = setHeader ("Content-Security-Policy", Text.encodeUtf8 cspValue)
