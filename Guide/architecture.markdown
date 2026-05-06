# Architecture

```toc

```

This section answers common questions regarding where to place your code. These recommendations are found to be working well at digitally induced.

In general, remember that all specific web app logic should stay in the `Web/` space. The `Application/` space is for sharing code across all your different applications. E.g. code shared between your web application and your admin back-end.

## Directory Structure

| File or Directory             | Purpose                                                                             |
| ----------------------------- | ----------------------------------------------------------------------------------- |
| Config/                       |                                                                                     |
| Config/Config.hs              | Configuration for the framework and your application                                |
| Config/nix/nixpkgs-config.nix | Configuration for the Nix package manager                                           |
| Application/                  | Your domain logic lives here                                                        |
| Application/Schema.sql        | Models and database tables are defined here                                         |
| Web/Controller                | Web application controllers                                                         |
| Web/View/                     | Web application HTML template files                                                 |
| Web/Types.hs                  | Central place for all web application types                                         |
| static/                       | Images, CSS, and JavaScript files                                                   |
| .ghci                         | Default config file for the Haskell interpreter                                     |
| .gitignore                    | List of files to be ignored by git                                                  |
| App.cabal, Setup.hs           | Config for the cabal package manager                                                |
| default.nix                   | Declares your app dependencies (like package.json for NPM or composer.json for PHP) |
| Makefile                      | Default config file for the make build system                                       |

## FAQ

##### Where to place a function I want to use in all my views?

If the function is only used in a single application and is a building block for your layout, place it in `Web/View/Layout.hs`. The module is already imported in all your views (just don't forget to add the function to the export list).

If the function is used across multiple applications or more like a helper function, place it in `Application/Helper/View.hs`. This module is also already included in your view files.

##### Where to place a function I want to use in all my controllers?

Place it in `Application/Helper/Controller.hs`. This module is already imported into your controllers.

##### Where to place a custom type?

Place it in `Web/Types.hs`.

##### Next to my main web application, I'm also building an admin back-end application. Where to place it?

An IHP project can consist of multiple applications. Run `new-application admin` inside a `nix-shell` to generate a new admin application. The logic for the new application is located in the `Admin/` directory. You can find it on the web at `http://localhost:8000/admin/` (all actions are prefixed with `/admin/`).

##### How to structure my CSS?

CSS files, as all your other static assets, should be placed in the `static` directory.

Create a `static/app.css`. In there use CSS imports to import your other style sheets. An example `app.css` could look like this:

```css
@import "/layout.css";
@import "/widget.css";
@import "/form.css";
@import "/button.css";
@import "/users.css";
```

###### Page-specific CSS rules

Place page-specific CSS used by e.g. views of the `Web.Controller.Users` controller in `users.css`. Use [currentViewId](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:currentViewId) to scope your CSS rules to the view.

Given the view:

```haskell
module Web.View.Projects.Show where

render = [hsx|
    <div id={currentViewId}>
        <h1>Hello World!</h1>
    </div>
|]
```

This will render like:

```html
<div id="projects-show">
    <h1>Hello World!</h1>
</div>
`
```

So in your `projects.css` you can just do rules like:

```css
#projects-show h1 {
    color: blue;
}
```

###### SASS & Webpack

We discourage the use of tools like SASS or Webpack because they have too much overhead.

###### Library CSS

CSS files from external libraries or components should be placed in `static/vendor/`.

##### How to structure my JavaScript Code?

JavaScript files, as well as your other static assets, should be placed in the `static` directory.

In general, we follow an approach where most of the business logic resides on the Haskell server. Only for small interactions, or client-side GUI niceness, we try to use a small isolated bit of JavaScript.

Your global, non-page specific, JavaScript code can be placed in `app.js`.

E.g. the `app.js` could look like this:

```javascript
$(document).on('ready turbolinks:load', function () {
    // This is called on the first page load *and* also when the page is changed by turbolinks.

    // We make sure also the bind our events only once, so if a user navigates to
    // another page and back, we won't try to bind the same event twice.
        // Init sortable.
    document.querySelectorAll('.js-my-selector').forEach(function (elem) {

        // Check if the element is already initialized.
        if (Boolean(elem.jsMySelectorInitialized) === false) {

            // Bind event handlers here.
            // ...

            // Mark the element as initialized, so we won't bind the event handler twice.
            elem.jsMySelectorInitialized = true;
        }
    });

});
```

In your `Web.View.Layout` just import the `app.js`:

```html
<script src="/app.js"></script>
```

###### Page-specific JavaScript

Place page-specific JavaScript used by e.g. views of the `Web.Controller.Users` controller in the `users.js`.

In the views, just import the JavaScript with `<script src="/users.js"></script>`.

###### Webpack

We discourage the use of Webpack or any other bundler because they have too much overhead. Of course, this advice only applies if you follow the approach to use as little JavaScript as possible.

###### Library JavaScript

JavaScript files from external libraries or components should be placed in `static/vendor/`. For simplicity, it might make sense to just download the JavaScript bundle of the library you want to use, and then just commit it into git instead of using NPM.

For more complex use-cases with lots of JavaScript, you should not follow this advice and just use NPM instead.

##### Where to place static images?

Place your images in the `static` folder. We recommend using SVG images.

## Request Lifecycle

Every HTTP request in IHP passes through a well-defined sequence of steps. Understanding this flow helps you debug issues, add middleware, and reason about where your code runs.

Here is the complete journey of an HTTP request:

```
                         HTTP Request
                              |
                              v
                    +-------------------+
                    |   Warp Server     |
                    +-------------------+
                              |
                              v
                    +-------------------+
                    | Middleware Stack   |
                    | (CORS, sessions,  |
                    |  method override, |
                    |  request parsing) |
                    +-------------------+
                              |
                              v
                    +-------------------+
                    | Static File Check |
                    | (static/ folder)  |
                    +---+----------+----+
                        |          |
                  (file found) (not found)
                        |          |
                        v          v
                   Return     +-------------------+
                   file       | FrontController    |
                              | (RootApplication)  |
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | Sub-Application   |
                              | FrontController   |
                              | (WebApplication)  |
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | Router / AutoRoute|
                              | (URL -> Action)   |
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | initContext        |
                              | (setup controller |
                              |  context)         |
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | beforeAction       |
                              | (auth checks, etc)|
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | action             |
                              | (your code runs)  |
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | render / redirect |
                              | (build response)  |
                              +-------------------+
                                       |
                                       v
                              +-------------------+
                              | Response flows    |
                              | back through      |
                              | middleware         |
                              +-------------------+
                                       |
                                       v
                               HTTP Response
```

### Step 1: Warp Receives the Request

IHP uses the Warp web server, a high-performance HTTP server written in Haskell. Warp listens on the configured port (default 8000 in development) and hands each incoming request to IHP as a WAI `Application`.

### Step 2: Middleware Stack

Before your application code sees the request, it passes through a chain of WAI middleware. Each middleware can inspect or modify the request, the response, or both. The middleware stack is assembled in `IHP.Server.initMiddlewareStack` and includes, in order:

1. **Custom middleware** -- any middleware you configure in `Config/Config.hs`
2. **CORS** -- Cross-Origin Resource Sharing headers (if configured)
3. **Method override** -- allows HTML forms to simulate PUT, PATCH, and DELETE requests by including a `_method` field
4. **Session** -- reads and writes encrypted session cookies (using `clientsession`)
5. **Approot** -- normalizes the application root URL
6. **View layout** -- applies your layout wrapper around rendered views
7. **Response headers** -- manages custom response headers set during request processing
8. **RLS context** -- prepares row-level security context for database queries
9. **Page head** -- manages `<head>` elements like page title and meta tags
10. **Modal container** -- manages modal dialog state
11. **Model context** -- makes the database connection pool available to your controllers
12. **Framework config** -- makes the framework configuration available
13. **Request body parsing** -- parses form data, JSON, and file uploads
14. **PGListener** -- makes the PostgreSQL LISTEN/NOTIFY listener available
15. **Asset path** -- rewrites asset URLs for cache busting in production

### Step 3: Static File Check

Before routing begins, IHP checks if the requested path matches a file in your `static/` directory. If a matching file is found, it is served directly without invoking any controller. This is a fast path that skips all application routing. In production, static files are served with long-lived cache headers; in development, caching is disabled so you always get the latest version.

### Step 4: FrontController Routing (RootApplication)

If the request is not a static file, it reaches the `FrontController` for `RootApplication`. This top-level router is defined in your project and typically just mounts one or more sub-applications:

```haskell
instance FrontController RootApplication where
    controllers = [ mountFrontController WebApplication ]
```

If you have multiple applications (e.g., a web front-end and an admin back-end), each one is mounted here. The URL prefix determines which sub-application handles the request.

### Step 5: Sub-Application FrontController (WebApplication)

The sub-application's `FrontController` instance lists all controllers it knows about. Each entry uses `parseRoute` to associate a controller type with its URL pattern:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage PostsAction
        , parseRoute @PostsController
        , parseRoute @CommentsController
        ]
```

IHP tries each entry in order. The first matching route wins. The router uses a two-phase strategy for performance: first, it checks a precomputed HashMap for exact path matches (O(1) lookup), and only falls back to Attoparsec URL parsing for dynamic routes with parameters.

### Step 6: AutoRoute Matches the URL to an Action

For controllers using `AutoRoute` (the default), IHP automatically maps URLs to action data constructors based on naming conventions. For example, given this controller type:

```haskell
data PostsController
    = PostsAction
    | NewPostAction
    | ShowPostAction { postId :: !(Id Post) }
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
```

The router maps URLs like `/Posts` to `PostsAction`, `/ShowPost?postId=...` to `ShowPostAction { postId = ... }`, and so on. It also enforces HTTP methods: `Create*` actions require POST, `Update*` actions require POST or PATCH, and `Delete*` actions require DELETE.

### Step 7: initContext Runs

Before your action code runs, IHP calls `initContext` from your application's `InitControllerContext` instance. This is where you set up shared controller state, such as loading the currently logged-in user:

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAuthentication @User
```

If `initContext` throws an exception (for example, if authentication redirects to a login page), the action is never called.

### Step 8: beforeAction Runs

Next, IHP calls the `beforeAction` method of the matched controller. This is a per-controller hook where you can run checks that apply to all actions in that controller, such as authorization:

```haskell
instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do ...
```

If `beforeAction` throws a response (e.g., a redirect), the action is skipped.

### Step 9: The Action Executes

Now your action function runs. This is where your application logic lives. You can query the database, read request parameters, set flash messages, and decide how to respond:

```haskell
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

### Step 10: Response Is Produced

The action typically ends by calling one of IHP's response functions:

- **`render SomeView { .. }`** -- renders an HTML view using HSX templates, wrapped in your layout
- **`renderJson someValue`** -- returns a JSON response
- **`renderPlain "text"`** -- returns plain text
- **`redirectTo SomeAction`** -- sends a 302 redirect to another action
- **`respondAndExit response`** -- sends a raw WAI response

The `render` function calls `beforeRender` on the view (a hook for last-minute modifications), then evaluates the `html` method of the `View` instance to produce HTML. The layout wrapper (set during `initContext`) wraps the view's HTML in your page layout.

### Step 11: Response Flows Back Through Middleware

The response travels back through the middleware stack in reverse order. Middleware can add headers, modify the response body, or log the request. Finally, Warp sends the HTTP response to the client.

## How the Code Generator Works

IHP includes built-in code generators that create boilerplate files for controllers, views, and actions. You can use these generators from the IHP development IDE or from the command line.

### Generating a Controller

When you generate a controller (for example, for a `posts` table), the generator creates the following files and modifications:

| File | What Happens |
|------|-------------|
| `Web/Controller/Posts.hs` | **Created.** Contains the `Controller PostsController` instance with action stubs for index, show, new, create, edit, update, and delete. |
| `Web/View/Posts/Index.hs` | **Created.** The index view showing a table of all posts. |
| `Web/View/Posts/New.hs` | **Created.** The new-record form view. |
| `Web/View/Posts/Edit.hs` | **Created.** The edit-record form view. |
| `Web/View/Posts/Show.hs` | **Created.** The show-record detail view. |
| `Web/Types.hs` | **Appended to.** Adds the `data PostsController` type with all action constructors. |
| `Web/Routes.hs` | **Appended to.** Adds `instance AutoRoute PostsController`. |
| `Web/FrontController.hs` | **Modified.** Adds `import Web.Controller.Posts` and adds `parseRoute @PostsController` to the controller list. |

The generated controller reads the database schema from `Application/Schema.sql` to produce smart defaults. If it finds a `posts` table, it generates actions that use the correct field names, includes `fill` calls for all user-editable columns, and adds basic validation (e.g., `nonEmpty` for required text fields, `isEmail` for email columns).

### Generating a View

When you generate a view independently (outside of a full controller generation), the generator:

1. **Creates** `Web/View/{ControllerName}/{ViewName}.hs` with a `data` type for the view and a `View` instance containing an HSX template.
2. **Creates** the `Web/View/{ControllerName}/` directory if it does not exist.
3. **Adds** an import for the new view module to the corresponding controller file.

For standard view names (`IndexView`, `ShowView`, `NewView`, `EditView`), the generator produces specialized templates with appropriate forms, tables, and detail layouts. For other view names, it produces a generic template.

### Generating an Action

When you add a single action to an existing controller, the generator:

1. **Adds** the action implementation to the existing controller file (`Web/Controller/{ControllerName}.hs`).
2. **Adds** the new action constructor to the controller's data type in `Web/Types.hs`.

For known action patterns (show, edit, update, create, delete), the generator produces appropriate boilerplate. For other action names, it generates a generic action body.

### Can You Modify Generated Code?

Yes. The generated code is yours to modify. The generators only write to files at the time of generation. After that, the files are ordinary Haskell source files that you are expected to customize.

### Can You Run the Generator Again?

The generators use `CreateFile` actions that create new files. If a file already exists, the generator will not overwrite it. Additions to existing files (like appending to `Web/Types.hs` or `Web/FrontController.hs`) are appended or inserted at specific markers, so running the generator for a different controller will not interfere with previously generated code.

## How Types Are Generated

IHP automatically generates Haskell types from your database schema. This is one of IHP's most distinctive features: your Haskell record types always match your database tables, with no manual synchronization needed.

### The Source: Application/Schema.sql

Your database schema lives in `Application/Schema.sql`. This file contains standard PostgreSQL `CREATE TABLE` statements:

```sql
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE DEFAULT NOW() NOT NULL
);
```

### The Output: build/Generated/Types.hs

When the schema compiler runs, it reads `Application/Schema.sql` and generates Haskell modules under `build/Generated/`. The main entry point is `build/Generated/Types.hs`, which re-exports all generated types. The generated code includes:

- **Record types** for each table (e.g., `data Post` with fields `id`, `title`, `body`, `createdAt`)
- **Primary key types** (e.g., `Id Post` as a newtype around `UUID`)
- **Enum types** for PostgreSQL `CREATE TYPE ... AS ENUM` statements
- **Instances** for database serialization, JSON encoding, form parameter parsing, default values, and more
- **Hasql statements** for common operations like insert, update, and fetch-by-id

The generated type modules are split across multiple files for faster compilation:

| Generated File | Contents |
|---|---|
| `build/Generated/Types.hs` | Re-exports all generated modules |
| `build/Generated/Enums.hs` | Haskell types for PostgreSQL enums |
| `build/Generated/ActualTypes.hs` | Re-exports all table-specific type modules |
| `build/Generated/ActualTypes/{TableName}.hs` | Record type and instances for a single table |
| `build/Generated/Statements.hs` | Re-exports all table-specific statement modules |
| `build/Generated/Statements/{TableName}.hs` | Hasql statements for a single table |

### When Regeneration Happens

The types are regenerated in the following situations:

- **During development:** The IHP dev server watches `Application/Schema.sql` for changes. When you modify the schema (either by editing the file directly or using the Schema Designer in the IHP IDE), the dev server automatically runs the schema compiler to regenerate types, then recompiles your application.
- **During production builds:** The `build-generated-code` command runs as part of `make` or the Nix build process. The Makefile rule is: `build/Generated/Types.hs: Application/Schema.sql` -- so types are regenerated whenever the schema file changes.
- **Manually:** You can run `build-generated-code` from within a `nix develop` shell at any time.

### Why You Should Never Edit build/Generated/Types.hs

The files under `build/Generated/` are overwritten every time the schema compiler runs. Any manual edits will be lost. If you need custom types, instances, or helper functions, place them in your own modules (such as `Web/Types.hs` or `Application/Helper/`).

The separation is deliberate: `Application/Schema.sql` is the single source of truth for your data model, and the generated types are a derived artifact that always stays in sync with it.
