# HSX

```toc
```

## Introduction

HSX can be written pretty much like normal HTML. You can write a HSX expression inside your Haskell code by wrapping it with `[hsx|YOUR HSX CODE|]`. HSX expressions are just a syntax for blaze html and thus are automatically escaped as described in the blaze documentation.

Because the HSX is parsed, you will get a syntax error when you type in invalid html.

### Inline Haskell

HSX can access Haskell variables wrapped with `{}` like this:

```haskell
let
    x :: Text = "World"
in
    [hsx|Hello {x}!|]
```

**If the variable is another hsx expression, a blaze html element, a text or string**: it is just included as you would expect.

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

HSX has special handling for boolean values to make it easy to deal with html boolean attributes like `disabled`, `readonly`, `checked`, etc.

You can write

```html
<input disabled={True}/>
```

as a short form for:

```html
<input disabled="disabled"/>
```

Writing `False`:

```html
<input disabled={False}/>
```

This will not render the attribute:

```html
<input/>
```

### Spread Values

For dynamic use cases you can use `{...attributeList}`:

```html
<div { ...[ ("data-my-attribute", "Hello World!") ] } />
<div { ...[ ("data-user-" <> tshow userId, tshow userFirstname) ] } />
<div { ...someVariable } />
```

### Special Elements: `<script>` and `<style>`

For `<script>` and `<style>` tags HSX applies some special handling. This only applies to tags with inline scripts or styles:

```html
No Special Handling:

<script src="/hello.js"/>
<link rel="stylesheet" href="layout.css"/>

Special Handling applies:

<script>alert("Hello");</script>
<style>h1 { color: blue; }</style>
```

Inside those tags using a haskell expression will not work:

```html
<script>{myHaskellExpr}</script>
```

This will just literally output the string `{myHaskellExpr}` without evaluating the haskell expression itself. This is because javascript usually uses `{}` for object expressions like `{ a: "hello" }`. The same applies to inline CSS inside `<style>` elements.

So using `{haskellVariables}` inside your javascript like this will not work:

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

Additionally HSX will not do the the usual escaping for style and script bodies, as this will make e.g. the
javascript unusuable.

### Syntax Rules

While most html is also valid HSX, there are some difference you need to be aware of:

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

Html Comments are supported and can be used like this:

```html
<div>
    <!-- Begin of Main Section -->
    <h1>Hello</h1>
</div>
```