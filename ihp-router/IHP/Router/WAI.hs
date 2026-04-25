{-|
Module: IHP.Router.WAI
Description: Public facade for plain WAI users of @ihp-router@

Re-exports the surface a plain WAI app needs to declare routes via the
@[routes|…|]@ quasi-quoter and dispatch incoming requests through the
trie:

  * 'routes' — the generic quasi-quoter from "IHP.Router.DSL.TH"
  * 'RouteTrie', 'routeTrieMiddleware' — the runtime dispatcher
  * 'UrlCapture', 'HasPath', 'pathTo', 'parseCapture', 'renderCapture' — types and class methods
    referenced from inside the splice
  * 'Segment', the standard non-empty path-segment newtype

Typical use:

@
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import IHP.Router.WAI
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)

data Routes = ListPosts | ShowPost { postId :: Int }
    deriving (Eq, Show)

[routes|Routes
GET /posts             ListPosts
GET /posts/{postId}    ShowPost
|]

dispatch :: Routes -> Application
dispatch ListPosts \_ respond = respond (responseLBS status200 [] "list")
dispatch (ShowPost { postId }) \_ respond =
    respond (responseLBS status200 [] (cs (show postId)))

main :: IO ()
main = run 3000 (routeTrieMiddleware (routesTrie dispatch) notFound)
  where
    notFound \_ respond = respond (responseLBS status404 [] "Not Found")
@

The @routesTrie@ binding is emitted by the splice (one per controller
type) — its name is the controller's @\<lowercaseFirst>Trie@.
-}
module IHP.Router.WAI
    ( -- * The DSL
      routes
      -- * Trie + middleware
    , RouteTrie
    , routeTrieMiddleware
      -- * URL captures
    , UrlCapture (..)
    , Segment (..)
      -- * URL generation
    , HasPath (..)
    ) where

import IHP.Router.DSL.TH (routes)
import IHP.Router.Trie (RouteTrie)
import IHP.Router.Middleware (routeTrieMiddleware)
import IHP.Router.Capture (UrlCapture (..), Segment (..))
import IHP.Router.UrlGenerator (HasPath (..))
