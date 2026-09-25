{-|
Minimal demonstration of @ihp-router@ as a standalone WAI library.

Run with @cabal run minimal-wai@ — the server listens on
@http://localhost:3000@. Try:

>  curl http://localhost:3000/
>  curl http://localhost:3000/posts/42
>  curl http://localhost:3000/posts/42/comments
>  curl http://localhost:3000/files/some/deeply/nested/path
>  curl http://localhost:3000/search?q=haskell
>  curl -X POST http://localhost:3000/posts        # → 405 Method Not Allowed
>  curl http://localhost:3000/nope                  # → 404 Not Found

The whole app builds with @build-depends: base, wai, warp, http-types,
text, bytestring, ihp-router@. **No IHP dependency.** CI verifies the
boundary holds: any future change that introduces an IHP import into
@ihp-router@ fails this build.
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS.Char8
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text.Lazy
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)

-- The whole public surface a plain WAI user needs.
import IHP.Router.WAI

-- | The application's route set. Records become path captures and
-- query parameters via the 'UrlCapture' typeclass; the splice picks up
-- the field names and types by reifying this datatype.
data MyRoutes
    = ListPosts
    | ShowPost      { postId :: Int }
    | ShowComments  { postId :: Int }
    | DownloadFile  { path :: Text }
    | Search        { q :: Text }
    deriving (Eq, Show)

-- The splice emits, per controller type:
--   instance HasPath MyRoutes
--   myRoutesTrie :: (MyRoutes -> Application) -> RouteTrie
[routes|MyRoutes
GET /                          ListPosts
GET /posts/{postId}            ShowPost
GET /posts/{postId}/comments   ShowComments
GET /files/{+path}             DownloadFile
GET /search?q                  Search
|]

-- | Per-route handler. Plain WAI 'Application's, dispatched by
-- 'myRoutesTrie' once it has matched a path.
dispatch :: MyRoutes -> Application
dispatch ListPosts _req respond =
    respond (responseLBS status200 [] "all posts\n")

dispatch (ShowPost { postId }) _req respond =
    respond (responseLBS status200 [] (encode ("post #" <> Text.pack (show postId))))

dispatch (ShowComments { postId }) _req respond =
    respond (responseLBS status200 [] (encode ("comments for #" <> Text.pack (show postId))))

dispatch (DownloadFile { path }) _req respond =
    respond (responseLBS status200 [] (encode ("file at: " <> path)))

dispatch (Search { q }) _req respond =
    respond (responseLBS status200 [] (encode ("searching for: " <> q)))

-- | Fall-through for paths the trie doesn't match.
notFound :: Application
notFound _req respond = respond (responseLBS status404 [] "Not Found\n")

-- | Encode a Text response body with a trailing newline.
encode :: Text -> LBS.ByteString
encode t = Text.Lazy.Encoding.encodeUtf8 (Text.Lazy.fromStrict t) <> LBS.Char8.pack "\n"

main :: IO ()
main = do
    putStrLn "Listening on http://localhost:3000"
    run 3000 (routeTrieMiddleware (myRoutesTrie dispatch) notFound)
