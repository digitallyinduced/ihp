{-# LANGUAGE OverloadedStrings, BangPatterns, QuasiQuotes #-}
module Main where

import Prelude
import qualified IHP.HSX.QQ as Blaze
import qualified IHP.HSX.ToHtml as BlazeToHtml
import qualified IHP.HSX.MarkupQQ as Direct
import qualified IHP.HSX.Markup as Direct
import qualified IHP.HSX.StrictMarkupQQ as Strict
import qualified IHP.HSX.StrictMarkup as Strict
import qualified IHP.HSX.TextMarkupQQ as TextB
import qualified IHP.HSX.TextMarkup as TextB
import qualified IHP.HSX.ShortMarkupQQ as Short
import qualified IHP.HSX.ShortMarkup as Short
import qualified IHP.HSX.LazyMarkupQQ as Lazy
import qualified IHP.HSX.LazyMarkup as Lazy
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as Text
import Test.Tasty.Bench
import Data.Int (Int64)

-- Force evaluation
forceLBS :: LBS.ByteString -> Int64
forceLBS = LBS.length

forceBS :: BS.ByteString -> Int
forceBS = BS.length

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

renderText :: TextB.Markup -> LBS.ByteString
renderText = TextB.renderMarkup
{-# NOINLINE renderText #-}

renderShort :: Short.Markup -> BS.ByteString
renderShort = Short.renderMarkupBS
{-# NOINLINE renderShort #-}

renderLazy :: Lazy.Markup -> LBS.ByteString
renderLazy = Lazy.renderMarkup
{-# NOINLINE renderLazy #-}

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
-- Text (Data.Text.Lazy.Builder) benchmarks
-- ============================================================

textStaticPage :: TextB.Markup
textStaticPage = [TextB.hsx|
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
{-# NOINLINE textStaticPage #-}

textDynamicText :: Text -> TextB.Markup
textDynamicText name = [TextB.hsx|
    <div class="container">
        <h1>Hello {name}</h1>
        <p>Welcome to your dashboard, {name}.</p>
    </div>
|]
{-# NOINLINE textDynamicText #-}

textManyDynamic :: Text -> Text -> Text -> Int -> TextB.Markup
textManyDynamic title author date count = [TextB.hsx|
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
{-# NOINLINE textManyDynamic #-}

textListItems :: [Text] -> TextB.Markup
textListItems items = [TextB.hsx|
    <ul class="list-group">
        {textForEach items textRenderItem}
    </ul>
|]
{-# NOINLINE textListItems #-}

textRenderItem :: Text -> TextB.Markup
textRenderItem item = [TextB.hsx|<li class="list-group-item">{item}</li>|]
{-# NOINLINE textRenderItem #-}

textForEach :: (TextB.ToHtml html) => [a] -> (a -> html) -> TextB.Markup
textForEach items f = mconcat $ map (TextB.toHtml . f) items
{-# INLINE textForEach #-}

textTableRow :: (Text, Text, Text) -> TextB.Markup
textTableRow (col1, col2, col3) = [TextB.hsx|
    <tr>
        <td>{col1}</td>
        <td>{col2}</td>
        <td>{col3}</td>
    </tr>
|]
{-# NOINLINE textTableRow #-}

textRenderTable :: [(Text, Text, Text)] -> TextB.Markup
textRenderTable rows = [TextB.hsx|
    <table class="table table-striped">
        <thead>
            <tr>
                <th>Name</th>
                <th>Email</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>
            {textForEach rows textTableRow}
        </tbody>
    </table>
|]
{-# NOINLINE textRenderTable #-}

-- ============================================================
-- Short (ShortByteString DList) benchmarks
-- ============================================================

shortStaticPage :: Short.Markup
shortStaticPage = [Short.hsx|
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
{-# NOINLINE shortStaticPage #-}

shortDynamicText :: Text -> Short.Markup
shortDynamicText name = [Short.hsx|
    <div class="container">
        <h1>Hello {name}</h1>
        <p>Welcome to your dashboard, {name}.</p>
    </div>
|]
{-# NOINLINE shortDynamicText #-}

shortManyDynamic :: Text -> Text -> Text -> Int -> Short.Markup
shortManyDynamic title author date count = [Short.hsx|
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
{-# NOINLINE shortManyDynamic #-}

shortListItems :: [Text] -> Short.Markup
shortListItems items = [Short.hsx|
    <ul class="list-group">
        {shortForEach items shortRenderItem}
    </ul>
|]
{-# NOINLINE shortListItems #-}

shortRenderItem :: Text -> Short.Markup
shortRenderItem item = [Short.hsx|<li class="list-group-item">{item}</li>|]
{-# NOINLINE shortRenderItem #-}

shortForEach :: (Short.ToHtml html) => [a] -> (a -> html) -> Short.Markup
shortForEach items f = mconcat $ map (Short.toHtml . f) items
{-# INLINE shortForEach #-}

shortTableRow :: (Text, Text, Text) -> Short.Markup
shortTableRow (col1, col2, col3) = [Short.hsx|
    <tr>
        <td>{col1}</td>
        <td>{col2}</td>
        <td>{col3}</td>
    </tr>
|]
{-# NOINLINE shortTableRow #-}

shortRenderTable :: [(Text, Text, Text)] -> Short.Markup
shortRenderTable rows = [Short.hsx|
    <table class="table table-striped">
        <thead>
            <tr>
                <th>Name</th>
                <th>Email</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>
            {shortForEach rows shortTableRow}
        </tbody>
    </table>
|]
{-# NOINLINE shortRenderTable #-}

-- ============================================================
-- Lazy (LBS.ByteString concatenation) benchmarks
-- ============================================================

lazyStaticPage :: Lazy.Markup
lazyStaticPage = [Lazy.hsx|
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
{-# NOINLINE lazyStaticPage #-}

lazyDynamicText :: Text -> Lazy.Markup
lazyDynamicText name = [Lazy.hsx|
    <div class="container">
        <h1>Hello {name}</h1>
        <p>Welcome to your dashboard, {name}.</p>
    </div>
|]
{-# NOINLINE lazyDynamicText #-}

lazyManyDynamic :: Text -> Text -> Text -> Int -> Lazy.Markup
lazyManyDynamic title author date count = [Lazy.hsx|
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
{-# NOINLINE lazyManyDynamic #-}

lazyListItems :: [Text] -> Lazy.Markup
lazyListItems items = [Lazy.hsx|
    <ul class="list-group">
        {lazyForEach items lazyRenderItem}
    </ul>
|]
{-# NOINLINE lazyListItems #-}

lazyRenderItem :: Text -> Lazy.Markup
lazyRenderItem item = [Lazy.hsx|<li class="list-group-item">{item}</li>|]
{-# NOINLINE lazyRenderItem #-}

lazyForEach :: (Lazy.ToHtml html) => [a] -> (a -> html) -> Lazy.Markup
lazyForEach items f = mconcat $ map (Lazy.toHtml . f) items
{-# INLINE lazyForEach #-}

lazyTableRow :: (Text, Text, Text) -> Lazy.Markup
lazyTableRow (col1, col2, col3) = [Lazy.hsx|
    <tr>
        <td>{col1}</td>
        <td>{col2}</td>
        <td>{col3}</td>
    </tr>
|]
{-# NOINLINE lazyTableRow #-}

lazyRenderTable :: [(Text, Text, Text)] -> Lazy.Markup
lazyRenderTable rows = [Lazy.hsx|
    <table class="table table-striped">
        <thead>
            <tr>
                <th>Name</th>
                <th>Email</th>
                <th>Role</th>
            </tr>
        </thead>
        <tbody>
            {lazyForEach rows lazyTableRow}
        </tbody>
    </table>
|]
{-# NOINLINE lazyRenderTable #-}

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

textDynAttrItems :: [Text] -> TextB.Markup
textDynAttrItems items = [TextB.hsx|
    <ul class="list-group">
        {textForEach items textDynAttrItem}
    </ul>
|]
{-# NOINLINE textDynAttrItems #-}

textDynAttrItem :: Text -> TextB.Markup
textDynAttrItem item = [TextB.hsx|<li class={item}>{item}</li>|]
{-# NOINLINE textDynAttrItem #-}

shortDynAttrItems :: [Text] -> Short.Markup
shortDynAttrItems items = [Short.hsx|
    <ul class="list-group">
        {shortForEach items shortDynAttrItem}
    </ul>
|]
{-# NOINLINE shortDynAttrItems #-}

shortDynAttrItem :: Text -> Short.Markup
shortDynAttrItem item = [Short.hsx|<li class={item}>{item}</li>|]
{-# NOINLINE shortDynAttrItem #-}

lazyDynAttrItems :: [Text] -> Lazy.Markup
lazyDynAttrItems items = [Lazy.hsx|
    <ul class="list-group">
        {lazyForEach items lazyDynAttrItem}
    </ul>
|]
{-# NOINLINE lazyDynAttrItems #-}

lazyDynAttrItem :: Text -> Lazy.Markup
lazyDynAttrItem item = [Lazy.hsx|<li class={item}>{item}</li>|]
{-# NOINLINE lazyDynAttrItem #-}

main :: IO ()
main = defaultMain
    [ bgroup "static page"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze) blazeStaticPage
        , bench "direct" $ nf (forceLBS . renderDirect) directStaticPage
        , bench "strict" $ nf (forceBS . renderStrict) strictStaticPage
        , bench "text"   $ nf (forceLBS . renderText) textStaticPage
        , bench "short"  $ nf (forceBS . renderShort) shortStaticPage
        , bench "lazy"   $ nf (forceLBS . renderLazy) lazyStaticPage
        ]
    , bgroup "dynamic text"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeDynamicText) "Alice"
        , bench "direct" $ nf (forceLBS . renderDirect . directDynamicText) "Alice"
        , bench "strict" $ nf (forceBS . renderStrict . strictDynamicText) "Alice"
        , bench "text"   $ nf (forceLBS . renderText . textDynamicText) "Alice"
        , bench "short"  $ nf (forceBS . renderShort . shortDynamicText) "Alice"
        , bench "lazy"   $ nf (forceLBS . renderLazy . lazyDynamicText) "Alice"
        ]
    , bgroup "many dynamic"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze) (blazeManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "direct" $ nf (forceLBS . renderDirect) (directManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "strict" $ nf (forceBS . renderStrict) (strictManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "text"   $ nf (forceLBS . renderText) (textManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "short"  $ nf (forceBS . renderShort) (shortManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        , bench "lazy"   $ nf (forceLBS . renderLazy) (lazyManyDynamic "Hello World" "Alice" "2024-01-15" 42)
        ]
    , bgroup "list 10"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeListItems) (replicate 10 "Item")
        , bench "direct" $ nf (forceLBS . renderDirect . directListItems) (replicate 10 "Item")
        , bench "strict" $ nf (forceBS . renderStrict . strictListItems) (replicate 10 "Item")
        , bench "text"   $ nf (forceLBS . renderText . textListItems) (replicate 10 "Item")
        , bench "short"  $ nf (forceBS . renderShort . shortListItems) (replicate 10 "Item")
        , bench "lazy"   $ nf (forceLBS . renderLazy . lazyListItems) (replicate 10 "Item")
        ]
    , bgroup "list 100"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeListItems) (replicate 100 "Item")
        , bench "direct" $ nf (forceLBS . renderDirect . directListItems) (replicate 100 "Item")
        , bench "strict" $ nf (forceBS . renderStrict . strictListItems) (replicate 100 "Item")
        , bench "text"   $ nf (forceLBS . renderText . textListItems) (replicate 100 "Item")
        , bench "short"  $ nf (forceBS . renderShort . shortListItems) (replicate 100 "Item")
        , bench "lazy"   $ nf (forceLBS . renderLazy . lazyListItems) (replicate 100 "Item")
        ]
    , bgroup "list 1000"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeListItems) (replicate 1000 "Item")
        , bench "direct" $ nf (forceLBS . renderDirect . directListItems) (replicate 1000 "Item")
        , bench "strict" $ nf (forceBS . renderStrict . strictListItems) (replicate 1000 "Item")
        , bench "text"   $ nf (forceLBS . renderText . textListItems) (replicate 1000 "Item")
        , bench "short"  $ nf (forceBS . renderShort . shortListItems) (replicate 1000 "Item")
        , bench "lazy"   $ nf (forceLBS . renderLazy . lazyListItems) (replicate 1000 "Item")
        ]
    , bgroup "table 100"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "direct" $ nf (forceLBS . renderDirect . directRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "strict" $ nf (forceBS . renderStrict . strictRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "text"   $ nf (forceLBS . renderText . textRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "short"  $ nf (forceBS . renderShort . shortRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        , bench "lazy"   $ nf (forceLBS . renderLazy . lazyRenderTable) (replicate 100 ("Alice", "alice@example.com", "Admin"))
        ]
    , bgroup "dyn attrs 100"
        [ bench "blaze"  $ nf (forceLBS . renderBlaze . blazeDynAttrItems) (replicate 100 "item-class")
        , bench "direct" $ nf (forceLBS . renderDirect . directDynAttrItems) (replicate 100 "item-class")
        , bench "strict" $ nf (forceBS . renderStrict . strictDynAttrItems) (replicate 100 "item-class")
        , bench "text"   $ nf (forceLBS . renderText . textDynAttrItems) (replicate 100 "item-class")
        , bench "short"  $ nf (forceBS . renderShort . shortDynAttrItems) (replicate 100 "item-class")
        , bench "lazy"   $ nf (forceLBS . renderLazy . lazyDynAttrItems) (replicate 100 "item-class")
        ]
    ]
