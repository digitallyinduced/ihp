module Apps.Web.View.Layout (defaultLayout, Html) where

import TurboHaskell.ViewPrelude
import TurboHaskell.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Apps.Web.Types
import Apps.Web.Routes
import qualified TurboHaskell.FrameworkConfig as FrameworkConfig
import Config ()

type Html = HtmlWithContext ViewContext

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
    <link rel="stylesheet" href="/app.css"/>

    {scripts}

    <title>App</title>
</head>
<body>
    <div class="container mt-4">
        {renderFlashMessages}
        {inner}
    </div>
</body>
|]

scripts = do
    when (isDevelopment FrameworkConfig.environment) [hsx|<script id="livereload-script" src="/livereload.js"></script>|]
    [hsx|
        <script src="/vendor/morphdom-umd.min.js"></script>
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/helpers.js"></script>
    |]
    when (isProduction FrameworkConfig.environment) [hsx|
            <script src="/vendor/turbolinks.js"></script>
            <script src="/vendor/morphdom-umd.min.js"></script>
            <script src="/vendor/turbolinksMorphdom.js"></script>
            <script src="/vendor/turbolinksInstantClick.js"></script>
        |]


metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="App"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="TODO"/>
    <meta property="og:description" content="TODO"/>
|]