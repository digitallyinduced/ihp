{-# LANGUAGE OverloadedStrings, BangPatterns, QuasiQuotes #-}
module Main where

import Prelude
import qualified IHP.HSX.QQ as Blaze
import qualified IHP.HSX.ToHtml as BlazeToHtml
import qualified IHP.HSX.MarkupQQ as Direct
import qualified IHP.HSX.Markup as Direct
import qualified IHP.HSX.StrictMarkupQQ as Strict
import qualified IHP.HSX.StrictMarkup as Strict
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as Builder (hPutBuilder)
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.Bench
import Control.DeepSeq (NFData(..), rnf)
import System.IO (Handle, IOMode(..), openBinaryFile)

-- Force evaluation
forceLBS :: LBS.ByteString -> ()
forceLBS = rnf

forceBS :: BS.ByteString -> ()
forceBS bs = bs `seq` ()

-- Renderers
renderBlaze :: Html -> LBS.ByteString
renderBlaze = Utf8.renderHtml
{-# NOINLINE renderBlaze #-}

renderDirect :: Direct.Markup -> LBS.ByteString
renderDirect = Direct.renderMarkup
{-# NOINLINE renderDirect #-}

renderDirectBS :: Direct.Markup -> BS.ByteString
renderDirectBS = Direct.renderMarkupBS
{-# NOINLINE renderDirectBS #-}

renderStrict :: Strict.Markup -> BS.ByteString
renderStrict = Strict.renderMarkupBS
{-# NOINLINE renderStrict #-}

-- Write renderers (simulate sending response via Handle)
writeBlazeToHandle :: Handle -> Html -> IO ()
writeBlazeToHandle h html = LBS.hPut h (renderBlaze html)
{-# NOINLINE writeBlazeToHandle #-}

writeDirectToHandle :: Handle -> Direct.Markup -> IO ()
writeDirectToHandle h markup = LBS.hPut h (renderDirect markup)
{-# NOINLINE writeDirectToHandle #-}

writeDirectHPutBuilder :: Handle -> Direct.Markup -> IO ()
writeDirectHPutBuilder h (Direct.Markup b) = Builder.hPutBuilder h b
{-# NOINLINE writeDirectHPutBuilder #-}

-- ============================================================
-- Blaze benchmarks
-- ============================================================

blazeStaticPage :: Html
blazeStaticPage = [Blaze.hsx|
    <div class="container">
        <nav class="navbar navbar-expand-lg">
            <a class="navbar-brand" href="/">IHP</a>
            <div class="navbar-nav">
                <a class="nav-link" href="/posts">Posts</a>
                <a class="nav-link" href="/users">Users</a>
                <a class="nav-link" href="/about">About</a>
            </div>
        </nav>
        <main class="mt-4">
            <h1>Welcome to IHP</h1>
            <p>This is a fully static page with no dynamic content.</p>
        </main>
        <footer class="mt-4">
            <p>Copyright 2024</p>
        </footer>
    </div>
|]
{-# NOINLINE blazeStaticPage #-}

blazeDynamicText :: Text -> Html
blazeDynamicText name = [Blaze.hsx|
    <div class="container">
        <h1>Hello {name}</h1>
        <p>Welcome to your dashboard, {name}.</p>
    </div>
|]
{-# NOINLINE blazeDynamicText #-}

blazeManyDynamic :: Text -> Text -> Text -> Int -> Html
blazeManyDynamic title author date count = [Blaze.hsx|
    <article class="post">
        <header>
            <h1>{title}</h1>
            <div class="meta">
                <span class="author">{author}</span>
                <span class="date">{date}</span>
                <span class="count">{show count}</span>
            </div>
        </header>
        <div class="content">
            <p>Article by {author} published on {date}.</p>
            <p>This article titled "{title}" has been viewed {show count} times.</p>
        </div>
    </article>
|]
{-# NOINLINE blazeManyDynamic #-}

blazeListItems :: [Text] -> Html
blazeListItems items = [Blaze.hsx|
    <ul class="list-group">
        {blazeForEach items blazeRenderItem}
    </ul>
|]
{-# NOINLINE blazeListItems #-}

blazeRenderItem :: Text -> Html
blazeRenderItem item = [Blaze.hsx|<li class="list-group-item">{item}</li>|]
{-# NOINLINE blazeRenderItem #-}

blazeForEach :: (BlazeToHtml.ToHtml html) => [a] -> (a -> html) -> Html
blazeForEach items f = mconcat $ map (BlazeToHtml.toHtml . f) items
{-# INLINE blazeForEach #-}

blazeTableRow :: (Text, Text, Text) -> Html
blazeTableRow (col1, col2, col3) = [Blaze.hsx|
    <tr>
        <td>{col1}</td>
        <td>{col2}</td>
        <td>{col3}</td>
    </tr>
|]
{-# NOINLINE blazeTableRow #-}

blazeRenderTable :: [(Text, Text, Text)] -> Html
blazeRenderTable rows = [Blaze.hsx|
    <table class="table table-striped">
        <thead>
            <tr>
                <th>Name</th>
                <th>Email</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>
            {blazeForEach rows blazeTableRow}
        </tbody>
    </table>
|]
{-# NOINLINE blazeRenderTable #-}

-- ============================================================
-- Direct (lazy Builder) benchmarks
-- ============================================================

directStaticPage :: Direct.Markup
directStaticPage = [Direct.hsx|
    <div class="container">
        <nav class="navbar navbar-expand-lg">
            <a class="navbar-brand" href="/">IHP</a>
            <div class="navbar-nav">
                <a class="nav-link" href="/posts">Posts</a>
                <a class="nav-link" href="/users">Users</a>
                <a class="nav-link" href="/about">About</a>
            </div>
        </nav>
        <main class="mt-4">
            <h1>Welcome to IHP</h1>
            <p>This is a fully static page with no dynamic content.</p>
        </main>
        <footer class="mt-4">
            <p>Copyright 2024</p>
        </footer>
    </div>
|]
{-# NOINLINE directStaticPage #-}

directDynamicText :: Text -> Direct.Markup
directDynamicText name = [Direct.hsx|
    <div class="container">
        <h1>Hello {name}</h1>
        <p>Welcome to your dashboard, {name}.</p>
    </div>
|]
{-# NOINLINE directDynamicText #-}

directManyDynamic :: Text -> Text -> Text -> Int -> Direct.Markup
directManyDynamic title author date count = [Direct.hsx|
    <article class="post">
        <header>
            <h1>{title}</h1>
            <div class="meta">
                <span class="author">{author}</span>
                <span class="date">{date}</span>
                <span class="count">{show count}</span>
            </div>
        </header>
        <div class="content">
            <p>Article by {author} published on {date}.</p>
            <p>This article titled "{title}" has been viewed {show count} times.</p>
        </div>
    </article>
|]
{-# NOINLINE directManyDynamic #-}

directListItems :: [Text] -> Direct.Markup
directListItems items = [Direct.hsx|
    <ul class="list-group">
        {directForEach items directRenderItem}
    </ul>
|]
{-# NOINLINE directListItems #-}

directRenderItem :: Text -> Direct.Markup
directRenderItem item = [Direct.hsx|<li class="list-group-item">{item}</li>|]
{-# NOINLINE directRenderItem #-}

directForEach :: (Direct.ToHtml html) => [a] -> (a -> html) -> Direct.Markup
directForEach items f = mconcat $ map (Direct.toHtml . f) items
{-# INLINE directForEach #-}

directTableRow :: (Text, Text, Text) -> Direct.Markup
directTableRow (col1, col2, col3) = [Direct.hsx|
    <tr>
        <td>{col1}</td>
        <td>{col2}</td>
        <td>{col3}</td>
    </tr>
|]
{-# NOINLINE directTableRow #-}

directRenderTable :: [(Text, Text, Text)] -> Direct.Markup
directRenderTable rows = [Direct.hsx|
    <table class="table table-striped">
        <thead>
            <tr>
                <th>Name</th>
                <th>Email</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>
            {directForEach rows directTableRow}
        </tbody>
    </table>
|]
{-# NOINLINE directRenderTable #-}

-- ============================================================
-- Strict (bytestring-strict-builder) benchmarks
-- ============================================================

strictStaticPage :: Strict.Markup
strictStaticPage = [Strict.hsx|
    <div class="container">
        <nav class="navbar navbar-expand-lg">
            <a class="navbar-brand" href="/">IHP</a>
            <div class="navbar-nav">
                <a class="nav-link" href="/posts">Posts</a>
                <a class="nav-link" href="/users">Users</a>
                <a class="nav-link" href="/about">About</a>
            </div>
        </nav>
        <main class="mt-4">
            <h1>Welcome to IHP</h1>
            <p>This is a fully static page with no dynamic content.</p>
        </main>
        <footer class="mt-4">
            <p>Copyright 2024</p>
        </footer>
    </div>
|]
{-# NOINLINE strictStaticPage #-}

strictDynamicText :: Text -> Strict.Markup
strictDynamicText name = [Strict.hsx|
    <div class="container">
        <h1>Hello {name}</h1>
        <p>Welcome to your dashboard, {name}.</p>
    </div>
|]
{-# NOINLINE strictDynamicText #-}

strictManyDynamic :: Text -> Text -> Text -> Int -> Strict.Markup
strictManyDynamic title author date count = [Strict.hsx|
    <article class="post">
        <header>
            <h1>{title}</h1>
            <div class="meta">
                <span class="author">{author}</span>
                <span class="date">{date}</span>
                <span class="count">{show count}</span>
            </div>
        </header>
        <div class="content">
            <p>Article by {author} published on {date}.</p>
            <p>This article titled "{title}" has been viewed {show count} times.</p>
        </div>
    </article>
|]
{-# NOINLINE strictManyDynamic #-}

strictListItems :: [Text] -> Strict.Markup
strictListItems items = [Strict.hsx|
    <ul class="list-group">
        {strictForEach items strictRenderItem}
    </ul>
|]
{-# NOINLINE strictListItems #-}

strictRenderItem :: Text -> Strict.Markup
strictRenderItem item = [Strict.hsx|<li class="list-group-item">{item}</li>|]
{-# NOINLINE strictRenderItem #-}

strictForEach :: (Strict.ToHtml html) => [a] -> (a -> html) -> Strict.Markup
strictForEach items f = mconcat $ map (Strict.toHtml . f) items
{-# INLINE strictForEach #-}

strictTableRow :: (Text, Text, Text) -> Strict.Markup
strictTableRow (col1, col2, col3) = [Strict.hsx|
    <tr>
        <td>{col1}</td>
        <td>{col2}</td>
        <td>{col3}</td>
    </tr>
|]
{-# NOINLINE strictTableRow #-}

strictRenderTable :: [(Text, Text, Text)] -> Strict.Markup
strictRenderTable rows = [Strict.hsx|
    <table class="table table-striped">
        <thead>
            <tr>
                <th>Name</th>
                <th>Email</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>
            {strictForEach rows strictTableRow}
        </tbody>
    </table>
|]
{-# NOINLINE strictRenderTable #-}

-- ============================================================
-- Real-world: forum thread listing page (modeled after ihp-forum)
-- ============================================================

data ThreadData = ThreadData
    { threadTitle :: !Text
    , threadBody :: !Text
    , threadAuthor :: !Text
    , threadTopic :: !Text
    , threadDate :: !Text
    }

sampleThreads :: Int -> [ThreadData]
sampleThreads n = take n $ cycle
    [ ThreadData "How to deploy IHP to production?" "I've been building an app with IHP and I'm ready to deploy. What are the recommended options for hosting?" "alice" "Deployment" "2024-01-15"
    , ThreadData "Best practices for database migrations" "When working with a team, what's the best workflow for handling Schema.sql changes and migrations?" "bob" "Database" "2024-01-14"
    , ThreadData "Custom middleware in IHP" "I need to add rate limiting to my API endpoints. How do I write custom WAI middleware that integrates with IHP?" "charlie" "Advanced" "2024-01-13"
    , ThreadData "HSX performance tips" "Are there any tips for making HSX templates render faster? My pages have large tables." "diana" "Performance" "2024-01-12"
    , ThreadData "WebSocket support in IHP" "Does IHP support WebSockets out of the box? I want to build a real-time chat feature." "eve" "Features" "2024-01-11"
    ]

-- Direct backend
directForumPage :: [ThreadData] -> Direct.Markup
directForumPage threads = [Direct.hsx|
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        <title>IHP Forum</title>
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    </head>
    <body>
        <nav class="navbar navbar-expand-lg navbar-light bg-light">
            <div class="container">
                <a class="navbar-brand" href="/">IHP Forum</a>
                <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarNav">
                    <span class="navbar-toggler-icon"></span>
                </button>
                <div class="collapse navbar-collapse" id="navbarNav">
                    <ul class="navbar-nav mr-auto">
                        <li class="nav-item active"><a class="nav-link" href="/">Start</a></li>
                        <li class="nav-item"><a class="nav-link" href="/Topics">Topics</a></li>
                    </ul>
                    <ul class="navbar-nav">
                        <li class="nav-item"><a class="nav-link" href="/NewThread">New Thread</a></li>
                        <li class="nav-item dropdown">
                            <a class="nav-link dropdown-toggle" href="#" data-toggle="dropdown">alice</a>
                            <div class="dropdown-menu dropdown-menu-right">
                                <a class="dropdown-item" href="/EditUser">Update Profile</a>
                                <a class="dropdown-item" href="/DeleteSession">Logout</a>
                            </div>
                        </li>
                    </ul>
                </div>
            </div>
        </nav>
        <div class="container mt-4">
            <div class="threads">
                {directForEach threads directRenderThread}
            </div>
        </div>
        <footer class="mt-5 py-4 bg-dark text-light">
            <div class="container">
                <div class="row">
                    <div class="col-md-4">
                        <h5>IHP Forum</h5>
                        <p>A community forum built with IHP</p>
                    </div>
                    <div class="col-md-4">
                        <h5>Links</h5>
                        <ul class="list-unstyled">
                            <li><a href="https://ihp.digitallyinduced.com/" class="text-light">IHP Website</a></li>
                            <li><a href="https://github.com/digitallyinduced/ihp" class="text-light">GitHub</a></li>
                            <li><a href="https://ihp.digitallyinduced.com/api-docs" class="text-light">API Docs</a></li>
                        </ul>
                    </div>
                    <div class="col-md-4">
                        <p class="text-muted">Powered by IHP &amp; Haskell</p>
                    </div>
                </div>
            </div>
        </footer>
        <script src="/vendor/jquery-3.6.0.slim.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/app.js"></script>
    </body>
    </html>
|]
{-# NOINLINE directForumPage #-}

directRenderThread :: ThreadData -> Direct.Markup
directRenderThread thread = [Direct.hsx|
    <div class="thread mb-3 p-3 border rounded">
        <a class="thread-title h5 text-decoration-none" href="/ShowThread">
            {thread.threadTitle}
        </a>
        <div class="thread-body text-muted mt-1">
            {thread.threadBody}
        </div>
        <div class="text-muted small mt-2">
            <a class="text-muted" href="/ShowUser">{thread.threadAuthor}</a>
            , {thread.threadDate}
            <span class="ml-1">in <a href="/ShowTopic" class="text-muted">{thread.threadTopic}</a></span>
        </div>
    </div>
|]
{-# NOINLINE directRenderThread #-}

-- Blaze backend
blazeForumPage :: [ThreadData] -> Html
blazeForumPage threads = [Blaze.hsx|
    <!DOCTYPE html>
    <html lang="en">
    <head>
        <meta charset="utf-8"/>
        <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
        <title>IHP Forum</title>
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    </head>
    <body>
        <nav class="navbar navbar-expand-lg navbar-light bg-light">
            <div class="container">
                <a class="navbar-brand" href="/">IHP Forum</a>
                <button class="navbar-toggler" type="button" data-toggle="collapse" data-target="#navbarNav">
                    <span class="navbar-toggler-icon"></span>
                </button>
                <div class="collapse navbar-collapse" id="navbarNav">
                    <ul class="navbar-nav mr-auto">
                        <li class="nav-item active"><a class="nav-link" href="/">Start</a></li>
                        <li class="nav-item"><a class="nav-link" href="/Topics">Topics</a></li>
                    </ul>
                    <ul class="navbar-nav">
                        <li class="nav-item"><a class="nav-link" href="/NewThread">New Thread</a></li>
                        <li class="nav-item dropdown">
                            <a class="nav-link dropdown-toggle" href="#" data-toggle="dropdown">alice</a>
                            <div class="dropdown-menu dropdown-menu-right">
                                <a class="dropdown-item" href="/EditUser">Update Profile</a>
                                <a class="dropdown-item" href="/DeleteSession">Logout</a>
                            </div>
                        </li>
                    </ul>
                </div>
            </div>
        </nav>
        <div class="container mt-4">
            <div class="threads">
                {blazeForEach threads blazeRenderThread}
            </div>
        </div>
        <footer class="mt-5 py-4 bg-dark text-light">
            <div class="container">
                <div class="row">
                    <div class="col-md-4">
                        <h5>IHP Forum</h5>
                        <p>A community forum built with IHP</p>
                    </div>
                    <div class="col-md-4">
                        <h5>Links</h5>
                        <ul class="list-unstyled">
                            <li><a href="https://ihp.digitallyinduced.com/" class="text-light">IHP Website</a></li>
                            <li><a href="https://github.com/digitallyinduced/ihp" class="text-light">GitHub</a></li>
                            <li><a href="https://ihp.digitallyinduced.com/api-docs" class="text-light">API Docs</a></li>
                        </ul>
                    </div>
                    <div class="col-md-4">
                        <p class="text-muted">Powered by IHP &amp; Haskell</p>
                    </div>
                </div>
            </div>
        </footer>
        <script src="/vendor/jquery-3.6.0.slim.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/app.js"></script>
    </body>
    </html>
|]
{-# NOINLINE blazeForumPage #-}

blazeRenderThread :: ThreadData -> Html
blazeRenderThread thread = [Blaze.hsx|
    <div class="thread mb-3 p-3 border rounded">
        <a class="thread-title h5 text-decoration-none" href="/ShowThread">
            {thread.threadTitle}
        </a>
        <div class="thread-body text-muted mt-1">
            {thread.threadBody}
        </div>
        <div class="text-muted small mt-2">
            <a class="text-muted" href="/ShowUser">{thread.threadAuthor}</a>
            , {thread.threadDate}
            <span class="ml-1">in <a href="/ShowTopic" class="text-muted">{thread.threadTopic}</a></span>
        </div>
    </div>
|]
{-# NOINLINE blazeRenderThread #-}

-- Dynamic attribute benchmarks
blazeDynAttrItems :: [Text] -> Html
blazeDynAttrItems items = [Blaze.hsx|
    <ul class="list-group">
        {blazeForEach items blazeDynAttrItem}
    </ul>
|]
{-# NOINLINE blazeDynAttrItems #-}

blazeDynAttrItem :: Text -> Html
blazeDynAttrItem item = [Blaze.hsx|<li class={item}>{item}</li>|]
{-# NOINLINE blazeDynAttrItem #-}

directDynAttrItems :: [Text] -> Direct.Markup
directDynAttrItems items = [Direct.hsx|
    <ul class="list-group">
        {directForEach items directDynAttrItem}
    </ul>
|]
{-# NOINLINE directDynAttrItems #-}

directDynAttrItem :: Text -> Direct.Markup
directDynAttrItem item = [Direct.hsx|<li class={item}>{item}</li>|]
{-# NOINLINE directDynAttrItem #-}

strictDynAttrItems :: [Text] -> Strict.Markup
strictDynAttrItems items = [Strict.hsx|
    <ul class="list-group">
        {strictForEach items strictDynAttrItem}
    </ul>
|]
{-# NOINLINE strictDynAttrItems #-}

strictDynAttrItem :: Text -> Strict.Markup
strictDynAttrItem item = [Strict.hsx|<li class={item}>{item}</li>|]
{-# NOINLINE strictDynAttrItem #-}

main :: IO ()
main = do
  devNull <- openBinaryFile "/dev/null" WriteMode
  defaultMain
    [ bgroup "static page"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze) blazeStaticPage
        , bench "direct" $ nf (forceLBS . renderDirect) directStaticPage
        , bench "strict" $ nf (forceBS . renderStrict) strictStaticPage
        ]
    , bgroup "dynamic text"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeDynamicText) "Alice"
        , bench "direct" $ nf (forceLBS . renderDirect . directDynamicText) "Alice"
        , bench "strict" $ nf (forceBS . renderStrict . strictDynamicText) "Alice"
        ]
    , bgroup "many dynamic"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze) (blazeManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "direct" $ nf (forceLBS . renderDirect) (directManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "strict" $ nf (forceBS . renderStrict) (strictManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        ]
    , bgroup "list 10"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeListItems) (replicate 10 "Item")
        , bench "direct" $ nf (forceLBS . renderDirect . directListItems) (replicate 10 "Item")
        , bench "strict" $ nf (forceBS . renderStrict . strictListItems) (replicate 10 "Item")
        ]
    , bgroup "list 100"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeListItems) (replicate 100 "Item")
        , bench "direct" $ nf (forceLBS . renderDirect . directListItems) (replicate 100 "Item")
        , bench "strict" $ nf (forceBS . renderStrict . strictListItems) (replicate 100 "Item")
        ]
    , bgroup "list 1000"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeListItems) (replicate 1000 "Item")
        , bench "direct" $ nf (forceLBS . renderDirect . directListItems) (replicate 1000 "Item")
        , bench "strict" $ nf (forceBS . renderStrict . strictListItems) (replicate 1000 "Item")
        ]
    , bgroup "table 100"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "direct" $ nf (forceLBS . renderDirect . directRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "strict" $ nf (forceBS . renderStrict . strictRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        ]
    , bgroup "dyn attrs 100"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeDynAttrItems) (replicate 100 "item-class")
        , bench "direct" $ nf (forceLBS . renderDirect . directDynAttrItems) (replicate 100 "item-class")
        , bench "strict" $ nf (forceBS . renderStrict . strictDynAttrItems) (replicate 100 "item-class")
        ]
    , bgroup "forum 20 threads"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeForumPage) (sampleThreads 20)
        , bench "direct" $ nf (forceLBS . renderDirect . directForumPage) (sampleThreads 20)
        , bench "direct-toBS" $ nf (forceBS . renderDirectBS . directForumPage) (sampleThreads 20)
        ]
    , bgroup "forum 100 threads"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeForumPage) (sampleThreads 100)
        , bench "direct" $ nf (forceLBS . renderDirect . directForumPage) (sampleThreads 100)
        , bench "direct-toBS" $ nf (forceBS . renderDirectBS . directForumPage) (sampleThreads 100)
        ]
    , bgroup "forum 20 write"
        [ bench "blaze"  $ nfAppIO (writeBlazeToHandle devNull . blazeForumPage) (sampleThreads 20)
        , bench "direct" $ nfAppIO (writeDirectToHandle devNull . directForumPage) (sampleThreads 20)
        , bench "direct-hPutBuilder" $ nfAppIO (writeDirectHPutBuilder devNull . directForumPage) (sampleThreads 20)
        ]
    , bgroup "forum 100 write"
        [ bench "blaze"  $ nfAppIO (writeBlazeToHandle devNull . blazeForumPage) (sampleThreads 100)
        , bench "direct" $ nfAppIO (writeDirectToHandle devNull . directForumPage) (sampleThreads 100)
        , bench "direct-hPutBuilder" $ nfAppIO (writeDirectHPutBuilder devNull . directForumPage) (sampleThreads 100)
        ]
    ]
