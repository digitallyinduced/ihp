# SEO

```toc

```

## Setting Page Titles

Every page on your site should have a unique, descriptive `<title>` tag. Search engines display this in their results, and it is one of the most important on-page SEO factors.

IHP provides a built-in system for managing page titles through the `setTitle` and `pageTitle` functions, which are available in every controller and view without any extra imports.

### Setting Up the Layout

First, make sure your `Web/View/Layout.hs` uses `pageTitleOrDefault` inside the `<head>` tag. This renders the page title that has been set for the current request, falling back to a default if none was set:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html>
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
    </head>
    <body>
        {inner}
    </body>
</html>
|]
```

The string `"My App"` is the fallback title shown when no page-specific title has been set.

### Setting a Title from a View

The most common approach is to call `setTitle` from the `beforeRender` hook of your view. This runs just before the layout wraps your HTML, so the layout can read the title you set:

```haskell
module Web.View.Posts.Show where

import Web.View.Prelude

data ShowView = ShowView { post :: Post }

instance View ShowView where
    beforeRender ShowView { post } = do
        setTitle post.title

    html ShowView { post } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>
    |]
```

### Setting a Title from a Controller Action

You can also call `setTitle` directly from a controller action. This is useful when the title depends on data you have already fetched:

```haskell
instance Controller PostsController where
    action ShowPostAction { postId } = do
        post <- fetch postId
        setTitle post.title
        render ShowView { .. }
```

### Setting an App-Wide Default Title

If you want every page in your application to start with a particular title (which individual views can then override), call `setTitle` from `initContext` in `Web/FrontController.hs`:

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        setTitle "My App - Built with IHP"
```

Any view that calls `setTitle` in its own `beforeRender` will override this default.

## Meta Description

The meta description is a short summary of a page's content. Search engines often display it as the snippet beneath the page title in results. Each page should have a unique, relevant description.

### Setting Up the Layout

Add `descriptionOrDefault` to the `<head>` section of your layout:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html>
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
        {descriptionOrDefault "Default description of your website for search engines."}
    </head>
    <body>
        {inner}
    </body>
</html>
|]
```

This renders a `<meta name="description" content="...">` tag. The string you pass is the fallback used when no page-specific description has been set.

### Setting a Description Per Page

Call `setDescription` from the `beforeRender` hook of your view:

```haskell
instance View ShowView where
    beforeRender ShowView { post } = do
        setTitle post.title
        setDescription post.summary

    html ShowView { .. } = [hsx|...|]
```

You can also call `setDescription` from a controller action, just like `setTitle`.

## Open Graph Tags

Open Graph (OG) tags control how your pages appear when shared on social media platforms like Facebook, Twitter/X, and LinkedIn. Without these tags, social platforms will try to guess what to show -- often with poor results.

IHP has built-in helpers for the most important OG tags: `og:title`, `og:description`, `og:type`, `og:url`, and `og:image`.

### Setting Up the Layout

Add the OG helper functions to the `<head>` section of your layout:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html>
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
        {descriptionOrDefault "Default description of your website."}
        {ogTitleOrDefault "My App"}
        {ogTypeOrDefault "website"}
        {ogDescriptionOrDefault "Default description of your website."}
        {ogUrl}
        {ogImage}
    </head>
    <body>
        {inner}
    </body>
</html>
|]
```

The `OrDefault` variants render the meta tag with a fallback value. The `ogUrl` and `ogImage` helpers only render their meta tag when a value has been explicitly set -- if you never call `setOGUrl` or `setOGImage`, no tag is output.

### Setting OG Tags Per Page

Call the setter functions from `beforeRender` in your view:

```haskell
instance View ShowView where
    beforeRender ShowView { post } = do
        setTitle post.title
        setDescription post.summary
        setOGTitle post.title
        setOGDescription post.summary
        setOGUrl (urlTo ShowPostAction { postId = post.id })

        case post.imageUrl of
            Just url -> setOGImage url
            Nothing -> pure ()

    html ShowView { .. } = [hsx|...|]
```

### Available Setter Functions

| Function | Renders | Example Value |
|---|---|---|
| `setOGTitle` | `<meta property="og:title" content="...">` | `"My Blog Post"` |
| `setOGDescription` | `<meta property="og:description" content="...">` | `"A summary of the post"` |
| `setOGType` | `<meta property="og:type" content="...">` | `"article"`, `"website"`, `"product"` |
| `setOGUrl` | `<meta property="og:url" content="...">` | `"https://example.com/posts/123"` |
| `setOGImage` | `<meta property="og:image" content="...">` | `"https://example.com/images/post.jpg"` |

### Twitter Card Tags

Twitter/X uses its own set of meta tags for link previews. These are not built into IHP, but you can add them manually in your layout or views using HSX:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html>
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
        {descriptionOrDefault "Default description."}
        {ogTitleOrDefault "My App"}
        {ogDescriptionOrDefault "Default description."}
        {ogUrl}
        {ogImage}
        <meta name="twitter:card" content="summary_large_image">
    </head>
    <body>
        {inner}
    </body>
</html>
|]
```

If you want per-page Twitter tags, you can use the same vault-key pattern described in the [Views documentation](view.html) for layout variables.

## Canonical URLs

A canonical URL tells search engines which version of a page is the "official" one. This is important when the same content can be reached through multiple URLs (for example, with and without query parameters, or with different sorting options). Without a canonical tag, search engines might index duplicate pages and dilute your ranking.

### Adding a Canonical Tag to the Layout

Since IHP does not have a built-in `setCanonical` helper, you can store the canonical URL in the WAI request vault and read it back in the layout.

First, declare a vault key and a tiny middleware to set it:

```haskell
-- Web/View/Layout.hs (or a shared module)
import qualified Data.Vault.Lazy as Vault
import Network.Wai (Request, vault)
import System.IO.Unsafe (unsafePerformIO)

newtype CanonicalUrl = CanonicalUrl Text

canonicalUrlVaultKey :: Vault.Key (IORef (Maybe CanonicalUrl))
canonicalUrlVaultKey = unsafePerformIO Vault.newKey
{-# NOINLINE canonicalUrlVaultKey #-}

setCanonical :: (?request :: Request) => Text -> IO ()
setCanonical url = case Vault.lookup canonicalUrlVaultKey (vault ?request) of
    Just ref -> writeIORef ref (Just (CanonicalUrl url))
    Nothing -> pure ()

currentCanonical :: (?request :: Request) => Maybe CanonicalUrl
currentCanonical = case Vault.lookup canonicalUrlVaultKey (vault ?request) of
    Just ref -> unsafePerformIO (readIORef ref)
    Nothing -> Nothing
```

Add `insertNewIORefVaultMiddleware canonicalUrlVaultKey Nothing` to your `Config/Config.hs` middleware stack so the IORef is created on every request.

In your layout, read the canonical URL from the vault and render it if present:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html>
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
        {descriptionOrDefault "Default description."}
        {canonicalTag}
    </head>
    <body>
        {inner}
    </body>
</html>
|]

canonicalTag :: (?request :: Request) => Html
canonicalTag = case currentCanonical of
    Just (CanonicalUrl url) -> [hsx|<link rel="canonical" href={url}>|]
    Nothing -> mempty
```

### Setting a Canonical URL from a Controller

Call `setCanonical` in your action:

```haskell
instance Controller PostsController where
    action ShowPostAction { postId } = do
        post <- fetch postId
        setCanonical (urlTo ShowPostAction { postId = post.id })
        render ShowView { .. }
```

This ensures that even if the page is accessed with extra query parameters like `?utm_source=twitter`, search engines know the clean URL is the canonical one.

## Sitemap

A sitemap is an XML file that lists all the important pages on your site. Search engines use it to discover and crawl your content more efficiently. IHP includes the `ihp-sitemap` package with built-in support for generating XML sitemaps.

### Setting Up the Sitemap

First, create a new controller file `Web/Controller/Sitemap.hs` with the following contents:

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
                    { url = urlTo $ ShowPostAction post.id
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

### SitemapLink Fields

Each `SitemapLink` has three fields:

| Field | Type | Description |
|---|---|---|
| `url` | `Text` | The full URL of the page. Use `urlTo` to generate it. |
| `lastModified` | `Maybe UTCTime` | When the page was last updated. Use `Nothing` to omit. |
| `changeFrequency` | `Maybe SitemapChangeFrequency` | A hint for how often the page changes. |

The available change frequencies are: `Always`, `Hourly`, `Daily`, `Weekly`, `Monthly`, `Yearly`, and `Never`.

### Including Multiple Resource Types

A real sitemap usually includes links to several different types of pages:

```haskell
instance Controller SitemapController where
    action SitemapAction = do
        posts <- query @Post |> fetch
        pages <- query @Page |> fetch

        let postLinks = posts |> map (\post ->
                SitemapLink
                    { url = urlTo $ ShowPostAction post.id
                    , lastModified = Just post.updatedAt
                    , changeFrequency = Just Weekly
                    })

        let pageLinks = pages |> map (\page ->
                SitemapLink
                    { url = urlTo $ ShowPageAction page.id
                    , lastModified = Just page.updatedAt
                    , changeFrequency = Just Monthly
                    })

        let homeLink = SitemapLink
                { url = urlTo WelcomeAction
                , lastModified = Nothing
                , changeFrequency = Just Daily
                }

        renderXmlSitemap (Sitemap (homeLink : postLinks <> pageLinks))
```

### Customizing the Sitemap Route

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

## robots.txt

The `robots.txt` file tells search engine crawlers which parts of your site they are allowed to crawl. It is also the standard place to point crawlers to your sitemap.

### Using a Static File

The simplest approach is to place a `robots.txt` file in your project's `static/` directory. IHP serves files from `static/` automatically, so the file will be available at `https://yourdomain.com/robots.txt`.

Create `static/robots.txt`:

```
User-agent: *
Allow: /

Sitemap: https://yourdomain.com/sitemap.xml
```

Replace `https://yourdomain.com` with your actual domain.

### Serving robots.txt Dynamically

If you need the robots.txt content to change based on the environment (for example, disallowing crawling on staging), you can serve it from a controller action.

Add a route type and controller:

```haskell
-- Web/Types.hs
data RobotsController = RobotsAction
    deriving (Eq, Show, Data)
```

```haskell
-- Web/Routes.hs
instance HasPath RobotsController where
    pathTo RobotsAction = "/robots.txt"

instance CanRoute RobotsController where
    parseRoute' = do
        string "/robots.txt"
        endOfInput
        pure RobotsAction
```

```haskell
-- Web/Controller/Robots.hs
module Web.Controller.Robots where

import Web.Controller.Prelude

instance Controller RobotsController where
    action RobotsAction = do
        let baseUrl = ?context.frameworkConfig.baseUrl
        renderPlain (cs $ "User-agent: *\nAllow: /\n\nSitemap: " <> baseUrl <> "/sitemap.xml\n")
```

Then register it in `Web/FrontController.hs`:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @RobotsController
        , parseRoute @SitemapController
        -- ...
        ]
```

This approach uses `renderPlain` to return a plain text response with the correct `text/plain` content type.

## Structured Data (JSON-LD)

Structured data helps search engines understand the content of your pages. By adding JSON-LD (JavaScript Object Notation for Linked Data) to your pages, you can enable rich results in search -- such as star ratings, breadcrumbs, FAQ accordions, and more.

### Adding JSON-LD to a Page

JSON-LD is added as a `<script type="application/ld+json">` tag in the page's HTML. You can include it directly in your view using HSX:

```haskell
instance View ShowView where
    html ShowView { post } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>

        <script type="application/ld+json">
            {preEscapedToHtml (renderJsonLd post)}
        </script>
    |]

renderJsonLd :: Post -> Text
renderJsonLd post = cs $ encode $ object
    [ "@context" .= ("https://schema.org" :: Text)
    , "@type" .= ("Article" :: Text)
    , "headline" .= post.title
    , "description" .= post.summary
    , "datePublished" .= post.createdAt
    , "dateModified" .= post.updatedAt
    ]
```

The `encode` function is from `Data.Aeson`, which is re-exported by the view prelude. The `preEscapedToHtml` function ensures the JSON is inserted into the page without HTML escaping.

### Common Schema Types

Here are a few commonly used Schema.org types:

- **Article** -- for blog posts and news articles
- **Product** -- for e-commerce product pages (can include price, availability, reviews)
- **FAQPage** -- for FAQ pages (can display as an accordion in Google results)
- **BreadcrumbList** -- for breadcrumb navigation
- **Organization** -- for your company/site info (often placed on the homepage)

Refer to [schema.org](https://schema.org/) for the full list of types and their required properties. Google's [Rich Results Test](https://search.google.com/test/rich-results) tool can validate your structured data.

### Organization Example for a Layout

If you want to include Organization structured data on every page, you can add it to your layout:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html>
    <head>
        <title>{pageTitleOrDefault "My App"}</title>
        {descriptionOrDefault "Default description."}
        <script type="application/ld+json">
            {preEscapedToHtml organizationJsonLd}
        </script>
    </head>
    <body>
        {inner}
    </body>
</html>
|]

organizationJsonLd :: Text
organizationJsonLd = cs $ encode $ object
    [ "@context" .= ("https://schema.org" :: Text)
    , "@type" .= ("Organization" :: Text)
    , "name" .= ("My Company" :: Text)
    , "url" .= ("https://example.com" :: Text)
    ]
```

## Bringing It All Together

Here is a complete layout example that incorporates all the SEO features described above:

```haskell
-- Web/View/Layout.hs

module Web.View.Layout where

import Web.View.Prelude

newtype CanonicalUrl = CanonicalUrl Text

defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en">
    <head>
        <meta charset="utf-8">
        <meta name="viewport" content="width=device-width, initial-scale=1">

        <title>{pageTitleOrDefault "My App"}</title>
        {descriptionOrDefault "A web application built with IHP."}

        {ogTitleOrDefault "My App"}
        {ogTypeOrDefault "website"}
        {ogDescriptionOrDefault "A web application built with IHP."}
        {ogUrl}
        {ogImage}

        <meta name="twitter:card" content="summary_large_image">

        {canonicalTag}
    </head>
    <body>
        {inner}
    </body>
</html>
|]

canonicalTag :: (?context :: ControllerContext) => Html
canonicalTag = case maybeFromFrozenContext @CanonicalUrl of
    Just (CanonicalUrl url) -> [hsx|<link rel="canonical" href={url}>|]
    Nothing -> mempty
```

And here is a view that sets all the relevant SEO metadata for a page:

```haskell
-- Web/View/Posts/Show.hs

module Web.View.Posts.Show where

import Web.View.Prelude
import Web.View.Layout (CanonicalUrl(..))

data ShowView = ShowView { post :: Post }

instance View ShowView where
    beforeRender ShowView { post } = do
        setTitle (post.title <> " - My App")
        setDescription post.summary
        setOGTitle post.title
        setOGDescription post.summary
        setOGUrl (urlTo ShowPostAction { postId = post.id })
        setOGType "article"

        case post.imageUrl of
            Just url -> setOGImage url
            Nothing -> pure ()

        setCanonical (urlTo ShowPostAction { postId = post.id })

    html ShowView { post } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>

        <script type="application/ld+json">
            {preEscapedToHtml (renderJsonLd post)}
        </script>
    |]

renderJsonLd :: (?context :: ControllerContext) => Post -> Text
renderJsonLd post = cs $ encode $ object
    [ "@context" .= ("https://schema.org" :: Text)
    , "@type" .= ("Article" :: Text)
    , "headline" .= post.title
    , "description" .= post.summary
    , "datePublished" .= post.createdAt
    , "dateModified" .= post.updatedAt
    ]
```
