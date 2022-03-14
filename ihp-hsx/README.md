# HSX

This `ihp-hsx` package provides `[hsx|<h1>Hello World</h1>|]` syntax to IHP projects.

To use it outside of IHP, add this package to your cabal file and use the modules like this:

```haskell
import IHP.HSX.QQ -- <- The main thing
import IHP.HSX.ConvertibleStrings () -- <- Helper instancess
import IHP.HSX.ToHtml () -- <- More helper instances

import Text.Blaze.Html5 -- The blaze html library, IHP HSX is built on top of that

view :: Html
view = [hsx|
    <h1>Hello World</h1>
|]

-- To render the above view:
import Text.Blaze.Html.Renderer.Text (renderHtml)

rendered :: Text
rendered = renderHtml view
```

## Introduction

HSX can be written pretty much like normal HTML. You can write an HSX expression inside your Haskell code by wrapping it with [`[hsx|YOUR HSX CODE|]`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:hsx). HSX expressions are just a syntax for [BlazeHtml](https://jaspervdj.be/blaze/) and thus are automatically escaped as described in the blaze documentation.

Because the HSX is parsed, you will get a syntax error when you type in invalid HTML.

### Inline Haskell

HSX can access Haskell variables wrapped with `{}` like this:

```haskell
let
    x :: Text = "World"
in
    [hsx|Hello {x}!|]
```

**If the variable is another HSX expression, a blaze HTML element, a text or string**: it is just included as you would expect.

**If the variable is any other custom Haskell data structure**: it will first be converted to a string representation by calling [`show`](https://ihp.digitallyinduced.com/api-docs/IHP-Prelude.html#v:show) on it. You can add a custom [`ToHtml`](https://ihp.digitallyinduced.com/api-docs/IHP-HSX-ToHtml.html#t:ToHtml) (import it from `IHP.HSX.ToHtml`) instance, to customize rendering a data structure.

You can also write more complex code like:

```haskell
let
    items :: [Int] = [ 0, 1, 2 ]
    renderItem n = [hsx|Hello {n}!|]
in
    [hsx|Complex demo: {forEach items renderItem}!|]
```

As the HSX expressions are compiled to Haskell code at compile-time, type errors inside these `{}` expressions will be reported to you by the compiler.

### Dynamic Attributes

The variable syntax can also be used in attribute values:

```haskell
let
    inputValue = "Hello World" :: Text
in
    [hsx|<input type="text" value={inputValue}/>|]
```

#### Boolean Attribute Values

HSX has special handling for Boolean values to make it easy to deal with HTML Boolean attributes like `disabled`, `readonly`, `checked`, etc.

You can write

```haskell
<input disabled={True} />
```

as a short form for:

```haskell
<input disabled="disabled" />
```

Writing `False`:

```haskell
<input disabled={False} />
```

This will not render the attribute, [as specified in the HTML standard](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes):

```haskell
<input />
```

#### Boolean data attributes

This behavior of omiting a attribute when it's set to `False` does not apply to `data-` attributes.

You can write

```haskell
<form data-disable-javascript-submission={True}/>
```

and it will render like this:

```html
<form data-disable-javascript-submission="true"/>
```

When set to `False`, like this:

```haskell
<form data-disable-javascript-submission={False}/>
```

the output HTML will keep the attribute and set it to `"false"`:

```html
<form data-disable-javascript-submission="false"/>
```

#### Maybe Attribute Values / Optional Attributes

HSX has special handling for Maybe values to make it easy to deal with optional attributes.

You can write

```haskell
let
    target :: Maybe Text
    target = Just "_blank"
in
    [hsx|<a target={target} />|]
```

and it will render to:

```haskell
<a target="_blank" />
```

Using `Nothing` results in the `target` attribute not being in the output HTML:

```haskell
let
    target :: Maybe Text
    target = Nothing
in
    [hsx|<a target={target} />|]
```

This will render to:

```haskell
<a />
```

### Spread Values

For dynamic use cases you can use `{...attributeList}`:

```haskell
<div { ...[ ("data-my-attribute" :: Text, "Hello World!" :: Text) ] } />
<div { ...[ ("data-user-" <> tshow userId, tshow userFirstname) ] } />
<div { ...someVariable } />
```

Note the `<>` concatenation operator.

### Special Elements: `<script>` and `<style>`

For `<script>` and `<style>` tags HSX applies some special handling. This only applies to tags with inline scripts or styles:

```html
<!-- No Special Handling: -->

<script src="/hello.js"></script>
<link rel="stylesheet" href="layout.css" />

<!-- Special Handling applies: -->

<script>
    alert("Hello");
</script>
<style>
    h1 {
        color: blue;
    }
</style>
```

Inside those tags using a Haskell expression will not work:

```haskell
<script>{myHaskellExpr}</script>
```

This will just literally output the string `{myHaskellExpr}` without evaluating the Haskell expression itself. This is because JavaScript usually uses `{}` for object expressions like `{ a: "hello" }`. The same applies to inline CSS inside `<style>` elements.

So using `{haskellVariables}` inside your JavaScript like this will not work:

```html
<script>
    var apiKey = "{apiKey}";
</script>
```

Instead use a `data-` attribute to solve this:

```html
<script data-api-key={apiKey}>
    var apiKey = document.currentScript.dataset.apiKey;
</script>
```

Additionally, HSX will not do the usual escaping for style and script bodies, as this will make e.g. the
JavaScript unusable.

### Syntax Rules

While most HTML is also valid HSX, there are some difference you need to be aware of:

#### Closing Tags

Tags always need to have a closing tag or be self-closing: so instead of `<br>` you need to write `<br/>`

#### JSX Differences

In JSX you have the restriction that you always have to have a single root tag. In HSX this restriction does not apply, so this is valid HSX (but not valid JSX):

```haskell
[hsx|
<div>A</div>
<div>B</div>
|]
```

#### Whitespace

Spaces and newline characters are removed where possible at HSX parse time.

#### Comments

HTML Comments are supported and can be used like this:

```html
<div>
    <!-- Begin of Main Section -->
    <h1>Hello</h1>
</div>
```

#### Empty Attributes

HSX allows you to write empty attributes like these:

```haskell
[hsx|
    <input disabled/>
|]
```

The underlying HTML library blaze currently does not support an empty HTML attribute. Therefore empty attributes are implemented by setting the attribute value to the attribute name. [This is valid HTML supported by all browsers.](https://html.spec.whatwg.org/multipage/common-microsyntaxes.html#boolean-attributes). Therefore the generated HTML looks like this:

```html
<input disabled="disabled" />
```

#### Unescaped Strings

If you use HTML entities, such as `&nbsp;` for a non-breaking space, you will notice they appear exactly like that. To output directly (i.e. unescaped) use the method `preEscapedToMarkup` from `Text.Blaze.Html5`.


## Common HSX Patterns

### Dealing with Maybe Values

When dealing with [`Maybe`](https://ihp.digitallyinduced.com/api-docs/IHP-Prelude.html#t:Maybe) values you sometimes want to conditionally render something. E.g. in react to render a tag with JSX you would do it like this:

```html
<p>Hi {user.name}</p>
{user.country && <p><small>from {user.country}</small></p>}
<p>Welcome!</p>
```

In HSX you usually write it like this:


```haskell
render user = [hsx|
    <p>Hi {get #name user}</p>
    {renderCountry}
|]
    where
        renderCountry = case get #country user of
            Just country -> [hsx|<p><small>{country}</small></p>|]
            Nothing -> [hsx||]
```

What about if the country is a empty string? A simple solution could look like this:

```haskell
renderCountry = case get #country user of
    Just "" -> [hsx||]
    Just country -> [hsx|<p>from {country}!</p>|]
    Nothing -> [hsx||]
```

That code doesn't feel right. It's better to deal with the problem at create time of the `User` record already. Use  [`emptyValueToNothing`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Param.html#v:emptyValueToNothing) to transform an empty `Just ""` to `Nothing` before inserting it into the db:

```haskell
action CreateUserAction = do
    newRecord @User
        |> fill '["name", "country"]
        |> emptyValueToNothing #country
        |> createRecord
```

Now when the country input field is empty when creating the user, the `country` field will be set to `Nothing` instead of `Just ""`.

**The `emptyValueToNothing` function is part of the IHP framework and not bundled in this ihp-hsx package, so you might need to implement it yourself if you're using HSX outside of an IHP app.**

### Displaying HTML without Escaping

In HSX all variables and expressions are automatically escaped to avoid [XSS](https://en.wikipedia.org/wiki/Cross-site_scripting).

Let's say we have variable `myVariable = "<script>alert(1)</script>"`. This is how the HSX will behave:

```html
<div>{myVariable}</div>

-- RENDERS to this HTML:
<div>%3Cscript%3Ealert%281%29%3C/script%3E</div>
```

Sometimes you want to insert some actual HTML code to your HSX using a variable. E.g. when your app is rendering markdown, the generated HTML by the markdown library should not be escaped, the markdown library typically already takes care of that.

In these cases you can use [`preEscapedToHtml`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:preEscapedToHtml) to mark a text as already escaped:

```html
<div>{markdownHtml |> preEscapedToHtml}</div>

-- Let's define `markdownHtml = "<p>hello</p>"`

-- This will render the following HTML:
<div><p>hello</p></div>
```

Be careful when using [`preEscapedToHtml`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:preEscapedToHtml) as it can easily introduce security issues.

The [`preEscapedToHtml`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:preEscapedToHtml) function can also be used to output HTML code that is not supported by HSX:
The [`preEscapedToHtml`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewPrelude.html#v:preEscapedToHtml) function can also be used to output HTML code that is not supported by HSX:

```html
{"<!--[if IE]> Internet Explorer Conditional Comments <![endif]-->" |> preEscapedToHtml}
```

## Example: HSX and the equivalent BlazeHtml

The following code using HSX:

```haskell
instance View EditView where
    html EditView { .. } = [hsx|
        <nav>
            <ol class="breadcrumb">
                <li class="breadcrumb-item"><a href={PostsAction}>Posts</a></li>
                <li class="breadcrumb-item active">Edit Post</li>
            </ol>
        </nav>
        <h1>Edit Post</h1>
        {renderForm post}
    |]
```

is roughly equivalent to the following BlazeHTML code:

```haskell
instance View EditView where
    html EditView { .. } = do
        H.nav $ do
            H.ol ! A.class_ "breadcrumb" $ do
                H.li ! A.class_ "breadcrumb-item" $ do
                    H.a ! A.href (fromString (P.show PostsAction)) $ do
                        "Posts"
                H.li ! A.class_ "breadcrumb-item active" $ do
                    "Edit Post"
        H.h1 "Edit Post"
        renderForm post
```

given the following imports:

```haskell
module Web.View.Posts.Edit where
import Web.View.Prelude

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Prelude as P
```
