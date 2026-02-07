{-|
Module: IHP.Controller.Layout
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.Controller.Layout
( setLayout
, getLayout
, ViewLayout (..)
, viewLayoutVaultKey
, viewLayoutMiddleware
) where

import Prelude
import IHP.ViewSupport
import IHP.Controller.Context
import Network.Wai (Request, Middleware, vault)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Vault.Lazy as Vault
import Data.IORef

-- | Wrapper for a layout function that will be applied to views
newtype ViewLayout = ViewLayout ((?context :: ControllerContext, ?request :: Request) => Layout)

-- | Vault key for storing the mutable layout IORef in each request
viewLayoutVaultKey :: Vault.Key (IORef ViewLayout)
viewLayoutVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE viewLayoutVaultKey #-}

-- | Middleware that initializes the layout IORef with the identity layout.
-- This must be installed in the middleware stack for setLayout/getLayout to work.
viewLayoutMiddleware :: Middleware
viewLayoutMiddleware app request respond = do
    ref <- newIORef (ViewLayout id)
    let request' = request { vault = Vault.insert viewLayoutVaultKey ref (vault request) }
    app request' respond

-- | Set the layout to be used when rendering views.
--
-- Example:
--
-- > instance InitControllerContext WebApplication where
-- >     initContext = do
-- >         setLayout defaultLayout
--
setLayout :: (?context :: ControllerContext) => ((?context :: ControllerContext, ?request :: Request) => Layout) -> IO ()
setLayout layout =
    case Vault.lookup viewLayoutVaultKey (vault ?context.request) of
        Just ref -> writeIORef ref (ViewLayout layout)
        Nothing -> error "viewLayoutMiddleware not installed. Add it to your middleware stack in Server.hs"
{-# INLINE setLayout #-}

-- | Get the current layout. Returns the identity layout if none was set.
{-# INLINE getLayout #-}
getLayout :: (?request :: Request) => IO ViewLayout
getLayout =
    case Vault.lookup viewLayoutVaultKey (vault ?request) of
        Just ref -> readIORef ref
        Nothing -> error "viewLayoutMiddleware not installed. Add it to your middleware stack in Server.hs"
