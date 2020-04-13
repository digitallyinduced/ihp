module TurboHaskell.IDE.ToolServer.Layout where

import TurboHaskell.ViewPrelude
import TurboHaskell.IDE.SchemaDesigner.Types
import TurboHaskell.IDE.ToolServer.Types
import TurboHaskell.IDE.ToolServer.Routes
import TurboHaskell.Environment
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type Html = HtmlWithContext ViewContext

toolServerLayout :: Html -> Html
toolServerLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, user-scalable=no"/>

    <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
    <link rel="stylesheet" href="/schema-designer.css"/>

    <script src="/vendor/morphdom-umd.min.js"></script>
    <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
    <script src="/vendor/timeago.js"></script>
    <script src="/vendor/popper.min.js"></script>
    <script src="/vendor/bootstrap.min.js"></script>
    <script src="/helpers.js"></script>
    <script src="/vendor/turbolinks.js"></script>
    <script src="/vendor/morphdom-umd.min.js"></script>
    <script src="/vendor/turbolinksMorphdom.js"></script>
    <script src="/vendor/turbolinksInstantClick.js"></script>

    <title>TurboHaskell</title>
</head>
<body class="d-flex h-100 flex-column">
    {inner}
</body>
|]

