# HSX

```toc
```

## Introduction

HSX can be written pretty much like normal HTML. You can write an HSX expression inside your Haskell code by wrapping it with `[hsx|YOUR HSX CODE|]`. HSX expressions are just a syntax for blaze HTML and thus are automatically escaped as described in the blaze documentation.

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

**If the variable is any other custom Haskell data structure**: it will first be converted to a string representation by calling `show` on it. You can add a custom `ToHtml` (import it from `IHP.HtmlSupport.ToHtml`) instance, to customize rendering a data structure.

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

### Boolean Attribute Values

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

This will not render the attribute:

```haskell
<input />
```

### Spread Values

For dynamic use cases you can use `{...attributeList}`:

```haskell
<div { ...[ ("data-my-attribute", "Hello World!") ] } />
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

This will just literally output the string `{myHaskellExpr}` without evaluating the Haskell expression itself. This is because javascript usually uses `{}` for object expressions like `{ a: "hello" }`. The same applies to inline CSS inside `<style>` elements.

So using `{haskellVariables}` inside your JavaScript like this will not work:

```html
<script>
    var apiKey = "{apiKey}";
</script>
```

Instead use a `data-` attribute to solve this:

```html
<script data-api-key="{apiKey}">
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
