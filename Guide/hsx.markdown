# HSX

```toc
```

## Introduction

HSX can be written pretty much like normal HTML. You can write a HSX expression inside your haskell code by wrapping it with `[hsx|YOUR HSX CODE|]`. HSX expressions are just a syntax for blaze html and thus are automatically escaped as described in the blaze documentation.

Because the HSX is parsed, you will get a syntax error when you type in invalid html.

### Inline Haskell

HSX can access haskell variables wrapped with `{}` like this:

```haskell
let
    x :: Text = "World"
in
    [hsx|Hello {x}!|]
```

**If the variable is another hsx expression a blaze html element, a text or string**: it is just included as it you will expect.

**If the variable is any other custom haskell data structure**: it will first be converted to a string representation by calling `show` on it. You can use add a custom `ToHtml` (import it from `IHP.HtmlSupport.ToHtml`) instance, to customize rendering a data structure.

You can also write more complex code like:

```haskell
let
    items :: [Int] = [ 0, 1, 2 ]
    renderItem n = [hsx|Hello {n}!|]
in
    [hsx|Complex demo: {forEach items renderItem}!|]
```

As the HSX expressions are compiled to haskell code at compile-time, type errors inside these `{}` expression will be reported to your by the compiler.

### Dynamic Attributes

The variable syntax can also be used in attribute values:

```haskell
let
    inputValue = "Hello World" :: Text
in
    [hsx|<input type="text" value={inputValue}/>|]
```

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
