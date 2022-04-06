# SEO

```toc

```

## Sitemap

Generating an XML Sitemap (`/sitemap.xml`)

First we create a new controller file: `Web/Controller/Sitemap.hs` with the following contents:

```haskell
module Web.Controller.Sitemap where

import Web.Controller.Prelude
import IHP.SEO.Sitemap.Types
import IHP.SEO.Sitemap.ControllerFunctions

instance Controller SitemapController where
    action SitemapAction = do
        -- Query all the posts
        posts <- query @Post |> fetch
        -- Build an `SitemapLink` for all posts
        let sitemapLinks = posts |> map (\post ->
                SitemapLink
                    { url = urlTo $ ShowPostAction (get #id post)
                    , lastModified = Nothing
                    , changeFrequency = Just Hourly
                    })
        -- Render The Sitemap
        renderXmlSitemap (Sitemap sitemapLinks)

```

In your `Web/Routes.hs` module, import the `IHP.SEO.Sitemap.Routes` module:

```haskell
module Web.Routes where
...
import IHP.SEO.Sitemap.Routes
...
```

Next, import the `IHP.SEO.Sitemap.Types` module in `Web/FrontController.hs`:

```haskell
module Web.FrontController where
...
import IHP.SEO.Sitemap.Types
...
```

And then add `parseRoute @SitemapController`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @SitemapController -- Add This Line
        -- Generator Marker
        ]
```

The `SitemapController` is configured by default to resolve `/sitemap.xml` routes.
If you need to customize the route, first, remove the `IHP.SEO.Sitemap.Routes` import from the `Web.Routes` module.
And add the following:

```haskell
module Web.Routes where
...
import IHP.SEO.Sitemap.Types -- Import The `SitemapController` Type
...

-- Here we customize the resolved route as `/custom-sitemap.xml`
instance HasPath SitemapController where
    pathTo SitemapAction = "/custom-sitemap.xml"

instance CanRoute SitemapController where
    parseRoute' = do
        string "/custom-sitemap.xml"
        endOfInput
        pure SitemapAction
```
