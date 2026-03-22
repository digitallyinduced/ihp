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
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.Bench
import Control.DeepSeq (NFData(..), rnf)

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

renderStrict :: Strict.Markup -> BS.ByteString
renderStrict = Strict.renderMarkupBS
{-# NOINLINE renderStrict #-}

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
main = defaultMain
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
    ]
