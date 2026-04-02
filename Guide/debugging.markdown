# Debugging

```toc

```

## Introduction

When something goes wrong in your IHP application, you will encounter one of three kinds of problems:

1. **Compiler errors** -- GHC rejects your code and shows an error message in the browser.
2. **Runtime errors** -- Your code compiles, but crashes or behaves unexpectedly when a request is made.
3. **Logic errors** -- Everything runs without crashing, but the result is not what you expected.

This guide walks you through the tools IHP provides for each situation. It assumes you know basic programming concepts but are new to Haskell.

## Reading Compiler Errors

When the IHP dev server detects a code change, it recompiles your project automatically. If GHC finds an error, the dev server shows it directly in your browser instead of your application. You will see a dark-themed error page with the file name, line number, and error details.

The most important part of any GHC error is the **first line**, which tells you the file, line, and column:

```
Web/Controller/Posts.hs:12:5: error:
```

This means the error is in `Web/Controller/Posts.hs` at line 12, column 5. You can click on the file name in the browser error overlay, and IHP will open the file in your editor at the correct line.

Here are the most common compiler errors you will encounter as a beginner.

### Variable Not in Scope

```
Web/Controller/Posts.hs:10:14: error:
    Variable not in scope: postsz :: IO [Post]
```

This means GHC cannot find a variable or function with the name `postsz`. Usually this is a typo. In this case, you probably meant `posts` instead of `postsz`.

It can also mean you forgot to import a module. For example, if you use `toLogStr` without importing it, you will see:

```
Not in scope: `toLogStr'
```

**Fix:** Add `import System.Log.FastLogger (toLogStr)` to the top of your module.

### Couldn't Match Type

```
Web/Controller/Posts.hs:15:12: error:
    Couldn't match type `[Char]' with `Text'
    Expected: Text
      Actual: [Char]
```

This means GHC expected one type but got a different one. In this example, a `String` (which GHC displays as `[Char]`) was used where `Text` was needed.

**Fix:** Use the `cs` function to convert between string-like types, or use the `OverloadedStrings` extension (already enabled in IHP) by making sure your string literals have the right type:

```haskell
-- This works because OverloadedStrings is enabled:
let greeting = "hello" :: Text

-- If you have a String variable, convert it:
let name = cs myStringVar :: Text
```

A very common variant is a type mismatch between your action and what the view expects:

```
Couldn't match type `Post' with `[Post]'
    Expected: [Post]
      Actual: Post
```

This tells you the view expects a list of posts, but you passed a single post (or the other way around). Check whether you used `fetch` (returns a list) or `fetchOne` (returns a single record).

### No Instance For

```
Web/View/Posts/Show.hs:8:10: error:
    No instance for (Show (Html)) arising from a use of `show'
```

This means you tried to use a function that requires a particular type class instance, but that instance does not exist. In this case, you tried to call `show` on an `Html` value, but `Html` does not have a `Show` instance.

**Fix:** Reconsider what you are trying to do. If you want to display a Haskell value inside HSX, you do not need `show`. Just put the value inside `{}`:

```haskell
[hsx|<p>{post.title}</p>|]
```

### Ambiguous Type Variable

```
Web/Controller/Posts.hs:9:14: error:
    Ambiguous type variable `a0' arising from a use of `fetch'
    Prevents the constraint `(Fetchable ... a0)' from being solved.
```

This means GHC cannot figure out which type you want. This often happens with `query` when GHC does not know which table you are querying.

**Fix:** Add a type application to tell GHC the type:

```haskell
-- Before (ambiguous):
records <- query |> fetch

-- After (explicit):
posts <- query @Post |> fetch
```

### Not in Scope: Data Constructor

```
Web/Controller/Posts.hs:5:20: error:
    Not in scope: data constructor `Post'
```

This means GHC cannot find a data constructor with that name. It usually means either:

- You forgot to import the module that defines it.
- There is a typo in the constructor name.
- The table does not exist in your `Application/Schema.sql` yet.

**Fix:** Make sure your controller imports `Web.Controller.Prelude` and that the table is defined in your schema. If you just added a new table, you may need to run the Schema Compiler by saving `Application/Schema.sql` in the Schema Designer.

### Parse Error in HSX

```
Web/View/Posts/Show.hs:12:9: error:
    Parse error in HSX at line 3, column 1
```

HSX is strict about valid HTML. Common causes:

- An unclosed tag: `<div>` without `</div>`
- A self-closing tag missing the slash: `<br>` should be `<br/>`
- Incorrect nesting: `<b><i>text</b></i>`
- Using a Haskell keyword as an attribute without escaping

**Fix:** Check your HSX for valid HTML structure. Remember that HSX requires XHTML-style self-closing tags:

```haskell
-- Wrong:
[hsx|<input type="text" name="title">|]

-- Right:
[hsx|<input type="text" name="title"/>|]
```

### Unbound Implicit Parameter

```
Web/Controller/Posts.hs:7:29: error:
    Unbound implicit parameter (?modelContext::ModelContext)
      arising from a use of `fetch'
```

IHP uses implicit parameters to pass context like the database connection. If you call a database function from a helper function outside of your controller action, you need to add the implicit parameter to your type signature.

**Fix:** Add `(?modelContext :: ModelContext) =>` to your function's type signature:

```haskell
-- Before:
fetchActiveUsers :: IO [User]
fetchActiveUsers = query @User |> filterWhere (#isActive, True) |> fetch

-- After:
fetchActiveUsers :: (?modelContext :: ModelContext) => IO [User]
fetchActiveUsers = query @User |> filterWhere (#isActive, True) |> fetch
```

See also the [Troubleshooting Guide](troubleshooting.html) for more details on this error.

## Printing Debug Output

When your code compiles but does not behave as expected, you need to inspect values at runtime. IHP gives you several ways to do this.

### Using `Debug.Trace`

The simplest way to print a value during execution is `Debug.Trace.traceShowId`. It prints a value to the terminal and returns it unchanged, so you can insert it into any expression without changing your code's behavior:

```haskell
action ShowPostAction { postId } = do
    post <- fetch postId
    let title = traceShowId post.title  -- prints the title to the terminal
    render ShowView { .. }
```

`traceShowId` works on any value that has a `Show` instance. The output appears in the terminal where `devenv up` is running, not in the browser.

You can also use `trace` to print a custom message:

```haskell
action CreatePostAction = do
    let post = newRecord @Post
    post
        |> fill @'["title", "body"]
        |> trace "About to validate"  -- prints "About to validate" and returns the post
        |> validateField #title nonEmpty
        |> ifValid \case
            Left post -> render NewView { .. }
            Right post -> do
                post <- post |> createRecord
                redirectTo PostsAction
```

IHP also provides a `debug` function (exported from the IHP Prelude) which is an alias for `traceShowId`:

```haskell
let result = debug myValue  -- same as traceShowId myValue
```

### Using `putStrLn` in Controllers

Inside a controller action (which runs in `IO`), you can use `putStrLn` directly:

```haskell
action ShowPostAction { postId } = do
    putStrLn "ShowPostAction called"
    post <- fetch postId
    putStrLn ("Fetched post: " <> post.title)
    render ShowView { .. }
```

The output appears in the terminal where `devenv up` is running.

### Using the Logger (Recommended)

For more structured output, use the fast-logger based logging system. Import it at the top of your module:

```haskell
import System.Log.FastLogger (toLogStr)
```

Then log via `?context.logger`:

```haskell
action ShowPostAction { postId } = do
    ?context.logger (toLogStr ("ShowPostAction called with postId: " <> show postId :: Text))
    post <- fetch postId
    ?context.logger (toLogStr ("Rendering post: " <> post.title))
    render ShowView { .. }
```

See the [Logging Guide](logging.html) for more details.

### Quick Reference: Which Logging Tool to Use

| Situation | Tool | Where Output Appears |
|-----------|------|---------------------|
| Quick throwaway debugging | `traceShowId` / `debug` | Terminal (stderr) |
| Debugging in controller actions | `putStrLn` | Terminal (stdout) |
| Structured, permanent logging | `?context.logger` | Terminal (stdout) |
| Inspecting a value inline without changing code flow | `traceShowId` | Terminal (stderr) |

## Using the Dev Server Error Overlay

The IHP dev server provides a browser-based error overlay that helps you during development. Here is how it works.

### Compilation Errors

When your code has a compiler error, the dev server intercepts the request and shows the error in the browser instead of your application. The page has a dark background and displays:

- The text "Problems found while compiling" at the top
- Each error with the file name as a clickable link (opens the file in your editor)
- Warnings sorted below errors so the most important issue is always first

The page **automatically updates via WebSocket**. When you save a file and the code recompiles successfully, the browser automatically switches back to your application. There is no need to manually refresh.

### Runtime Errors

When your code compiles but throws an exception at runtime, IHP shows a styled error page with:

- The exception message as the title
- A "Possible Solutions" section with specific advice
- The action that was running when the error occurred
- Links to community help (Slack, Stack Overflow, GitHub)

Different types of runtime errors get specialized error pages:

- **Database errors**: Shows the SQL query that failed, the PostgreSQL error code, and suggests running "Migrate DB" if a table or column is missing.
- **Record not found**: Shows the SQL query and suggests using `fetchOneOrNothing` instead of `fetchOne`.
- **Parameter not found**: Shows which parameter was missing and lists all parameters that were provided in the request.
- **Pattern match failure**: Suggests adding a missing action handler or checking a partial pattern match.
- **Routing errors**: Explains HTTP method mismatches (e.g., using a GET link for a DELETE action) and shows how to fix them.

These helpful error pages only appear in development mode. In production, users see a generic error message that does not leak internal details.

## Interactive Debugging with GHCi

GHCi (the GHC interactive interpreter) is one of the most powerful debugging tools available. You can use it to type-check code, test functions, and explore types without waiting for a full compilation cycle.

### Starting GHCi

From your project directory (with `devenv up` running in another terminal):

```bash
ghci
```

IHP's `.ghci` configuration file automatically loads the right extensions and modules.

### Checking Types

Use `:t` to check the type of any expression:

```
ghci> :t fetch
fetch :: (... ) => fetchable -> IO result

ghci> :t query @Post
query @Post :: QueryBuilder "posts"
```

### Getting Info About Types

Use `:i` to see the definition of a type, its instances, and where it was defined:

```
ghci> :i Post
data Post = Post { id :: Id Post, title :: Text, body :: Text, ... }
    -- Defined in 'Generated.Types'
```

### Loading and Testing Modules

You can load a specific module to check it for errors:

```
ghci> :l Web/Controller/Posts.hs
```

If there are errors, GHCi will show them. After fixing the errors, reload with:

```
ghci> :r
```

### Testing Functions

You can test pure functions directly in GHCi:

```
ghci> import Data.Text as Text
ghci> Text.toUpper "hello"
"HELLO"

ghci> Text.isPrefixOf "http" "https://example.com"
True
```

### Quick Type-Checking Workflow

A fast workflow for catching errors is:

1. Make a change in your editor.
2. In GHCi, type `:r` to reload.
3. GHCi shows any errors immediately (often faster than the dev server).
4. Fix and repeat.

This is especially useful when working on complex type-level code or refactoring multiple files.

## Inspecting Database Queries

IHP automatically logs all SQL queries to the terminal when the log level is set to `Debug` (the default in development). This helps you understand what queries the `QueryBuilder` generates.

### Reading Query Logs

When you make a request, you will see output like this in the terminal where `devenv up` is running:

```
[28-Jan-2025 10:15:32] Query (3ms): SELECT posts.id, posts.title, posts.body, posts.created_at FROM posts ORDER BY posts.created_at DESC
```

Each log entry shows:
- The timestamp
- The query execution time in milliseconds
- The full SQL query

### Matching QueryBuilder to SQL

Here is how common `QueryBuilder` expressions map to SQL:

```haskell
-- This Haskell code:
query @Post
    |> filterWhere (#title, "Hello")
    |> orderByDesc #createdAt
    |> fetch

-- Generates this SQL:
-- SELECT posts.id, posts.title, posts.body, posts.created_at
-- FROM posts
-- WHERE posts.title = 'Hello'
-- ORDER BY posts.created_at DESC
```

### Disabling Query Logging

If the query log output is too noisy, you can silence it for specific queries:

```haskell
import IHP.ModelSupport (withoutQueryLogging)

action PostsAction = do
    -- This query will not be logged:
    posts <- withoutQueryLogging (query @Post |> fetch)
    render IndexView { .. }
```

Or change the log level in `Config/Config.hs` to suppress debug messages entirely. See the [Logging Guide](logging.html) for details.

## Browser Developer Tools

Your browser's developer tools are essential for debugging the client-side behavior of your IHP application.

### Network Tab

Open the Network tab (F12 or Cmd+Option+I) to inspect:

- **Form submissions**: Check that form data is being sent correctly. Look at the request body to verify field names and values match what your controller expects.
- **Redirects**: After a successful form submission, IHP typically redirects. The Network tab shows the redirect chain.
- **Response codes**: A `200` means success, `400` means a routing or parameter error, `500` means a server-side exception.

### WebSocket Tab (for AutoRefresh)

If you use IHP's [AutoRefresh](auto-refresh.html) feature, the browser maintains a WebSocket connection to the server. In the Network tab, filter by "WS" to see WebSocket frames. This helps you verify that:

- The WebSocket connection is established
- The server sends updates when data changes
- The connection reconnects after a disconnection

### Console Tab

Check the browser console for JavaScript errors. IHP includes `helpers.js` which handles things like `js-delete` links and morphdom-based page updates. If a delete button does not work, the console often shows the reason.

## Common Errors and Solutions

Here is a quick reference table of common errors and how to fix them.

| Error Message | Cause | Fix |
|---|---|---|
| `Variable not in scope: myFunction` | Typo or missing import | Check spelling. Add the correct `import` statement. |
| `Not in scope: data constructor 'Post'` | Missing import or table not in schema | Ensure `Web.Controller.Prelude` is imported. Check `Application/Schema.sql`. |
| `Couldn't match type 'Text' with '[Char]'` | String type mismatch | Use `cs` to convert, or add a type annotation like `:: Text`. |
| `Couldn't match type 'Post' with '[Post]'` | Single record vs. list mismatch | Use `fetchOne` for a single record or `fetch` for a list. |
| `No instance for (Show Html)` | Trying to `show` a value that has no `Show` instance | Use `{value}` inside HSX instead of `show`. |
| `Ambiguous type variable` | GHC cannot infer which type you mean | Add a type application: `query @Post`. |
| `Parse error in HSX` | Invalid HTML in your template | Check for unclosed tags, use self-closing tags like `<br/>`. |
| `Unbound implicit parameter (?modelContext::ModelContext)` | Database function used outside controller without type signature | Add `(?modelContext :: ModelContext) =>` to your function signature. |
| `Unbound implicit parameter (?context::ControllerContext)` | Controller function used in a helper without the context | Add `(?context :: ControllerContext) =>` to your function signature. |
| `No response returned in SomeAction` | Your action does not call `render` or `redirectTo` | Add `render MyView { .. }` or `redirectTo SomeAction` at the end of your action. |
| `Parameter 'someParam' not found` | The request does not include the expected parameter | Check your form field names or URL query parameters. Use `paramOrDefault` for optional params. |
| `Call to fetchOne failed. No records returned.` | `fetchOne` found no matching row | Use `fetchOneOrNothing` and handle the `Nothing` case. |
| `Action was called from a GET request, but needs DELETE` | A link to a delete action is missing `class="js-delete"` | Add `class="js-delete"` to your link tag. |
| `Action was called from a GET request, but needs POST` | Linking directly to a create/update action | Use a `formFor` or `<form method="POST">` instead of a link. |
| `relation "posts" does not exist` | Table missing from database | Run "Migrate DB" in the IHP IDE, or check your `Application/Schema.sql`. |
| `column "title" does not exist` | Column missing from database | Run "Migrate DB" after updating your schema. |
| Connection refused on port 8000 | Dev server is not running | Start it with `devenv up`. |
| Connection refused on port 5432 | PostgreSQL is not running | Start it with `devenv up`. PostgreSQL starts automatically. |

## Getting Help

If you are stuck on an issue not covered here:

- Check the [Troubleshooting page](troubleshooting.html) for environment and setup issues.
- Ask on the [IHP Slack community](https://ihp.digitallyinduced.com/Slack).
- Search [Stack Overflow with the "ihp" tag](https://stackoverflow.com/questions/tagged/ihp).
- [Open a GitHub issue](https://github.com/digitallyinduced/ihp/issues/new) if you think you found a bug.
