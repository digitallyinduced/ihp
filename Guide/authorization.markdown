# Authorization

```toc

```

## Restricting an action to logged-in users

To restrict an action to a logged-in user, use [`ensureIsUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:ensureIsUser):

```haskell
action PostsAction = do
    ensureIsUser
    posts <- query @Post |> fetch
    render IndexView { .. }
```

When someone is trying to access the `PostsAction` but is not logged-in, the browser will be redirected to the login page. After the login succeeded, the user will be redirected back to the `PostsAction`.

It's common to restrict all actions inside a controller to logged-in users only. Place the [`ensureIsUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:ensureIsUser) inside the [`beforeAction`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#v:beforeAction) hook to automatically apply it to all actions:

```haskell
instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }
```

In this case `PostsAction` and `ShowPostAction` are only accessible to logged-in users.

## Restricting an action to logged-in admins

To restrict an action to a logged-in admin, use [`ensureIsAdmin`](https://ihp.digitallyinduced.com/api-docs/IHP-ControllerSupport.html#v:beforeAction) instead of [`ensureIsUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:ensureIsUser). If you get

```
error:
    * Could not deduce (HasNewSessionUrl admin0)
        arising from a use of `ensureIsAdmin'
[…]
      These potential instance exist:
        instance HasNewSessionUrl Admin -- Defined in `Admin.Types'
```

then you may have to annotate the type with `@Admin`. For example:

```haskell
instance Controller UserController where
    beforeAction =
        ensureIsAdmin @Admin
```

## Checking for Permissions

You can use [`accessDeniedUnless`](https://ihp.digitallyinduced.com/api-docs/IHP-AuthSupport-Authorization.html#v:accessDeniedUnless) to allow certain things only for specific users. For example, to restrict a `ShowPostAction` only to the user who a post belongs to, use this:

```haskell
    action ShowPostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (post.userId == currentUserId)

        render ShowView { .. }
```

There is also [`accessDeniedWhen`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-AccessDenied.html#v:accessDeniedWhen), which denies access when the condition is `True` (the inverse of `accessDeniedUnless`):

```haskell
    action EditPostAction { postId } = do
        post <- fetch postId
        accessDeniedWhen (post.userId /= currentUserId)

        render EditView { .. }
```

Both functions stop action execution immediately and return a 403 Access Denied page to the browser. The action code after the check is never reached when access is denied.

You can also call [`renderAccessDenied`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-AccessDenied.html#v:renderAccessDenied) directly when you need to deny access unconditionally:

```haskell
    action SecretAction = do
        renderAccessDenied
```

## Protecting Entire Controllers

### All Actions Require Login

The most common pattern is to place `ensureIsUser` in `beforeAction` so that every action in the controller requires a logged-in user:

```haskell
instance Controller PostsController where
    beforeAction = ensureIsUser

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action CreatePostAction = do
        -- ...
```

The `beforeAction` function runs before every action in the controller. If the user is not logged in, the browser is redirected to the login page and the action code never runs.

### Exempting Specific Actions

Sometimes you want most actions in a controller to require login, but a few to be publicly accessible. You can use the `?theAction` implicit parameter inside `beforeAction` to check which action is being called and skip the authentication check:

```haskell
instance Controller PostsController where
    beforeAction =
        case ?theAction of
            PostsAction -> pure () -- Allow public access to the index
            ShowPostAction {} -> pure () -- Allow public access to show
            _ -> ensureIsUser -- All other actions require login

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action ShowPostAction { postId } = do
        post <- fetch postId
        render ShowView { .. }

    action CreatePostAction = do
        -- Only reachable by logged-in users
        let post = newRecord @Post
            |> set #userId currentUserId
        -- ...
```

The `?theAction` variable is automatically available in both `beforeAction` and `action`. It contains the current action value, so you can pattern match on it to decide what to do.

### Multiple Checks in beforeAction

You can combine authentication and authorization checks in `beforeAction`:

```haskell
instance Controller PostsController where
    beforeAction = do
        ensureIsUser
        -- Fetch and authorize in one place
        case ?theAction of
            EditPostAction { postId } -> do
                post <- fetch postId
                accessDeniedUnless (post.userId == currentUserId)
            DeletePostAction { postId } -> do
                post <- fetch postId
                accessDeniedUnless (post.userId == currentUserId)
            _ -> pure ()
```

This approach runs `ensureIsUser` first (redirecting to login if needed), then checks ownership for edit and delete actions. You still need to fetch the post again inside each action body if you need it for rendering, but the authorization check happens early.

## Role-Based Access Control

A common requirement is to restrict certain actions based on a user's role (e.g., admin vs. regular user). IHP does not prescribe a specific role system, but it is straightforward to build one using a PostgreSQL enum and the standard authorization helpers.

### Adding a Role Column

First, add a `user_role` enum and a `role` column to your `users` table. In the Schema Designer or directly in `Application/Schema.sql`:

```sql
CREATE TYPE user_role AS ENUM ('user', 'admin');

CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL,
    role user_role NOT NULL DEFAULT 'user'
);
```

After running the migration, IHP's schema compiler will generate a `UserRole` Haskell type with constructors `User` and `Admin`, and your `User` record will have a `role` field of that type.

### Checking Roles in Actions

You can check the current user's role with `accessDeniedUnless`:

```haskell
action CreatePostAction = do
    accessDeniedUnless (currentUser.role == Admin)

    let post = newRecord @Post
    -- ...
```

### Checking Roles in beforeAction

To restrict an entire controller to admins:

```haskell
instance Controller AdminPostsController where
    beforeAction = do
        ensureIsUser
        accessDeniedUnless (currentUser.role == Admin)

    action AdminPostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }
```

Note that `ensureIsUser` must come before any use of `currentUser`. The `ensureIsUser` call makes sure the user is logged in (redirecting to the login page if not). The `accessDeniedUnless` call then checks the role and returns a 403 page if the user is not an admin.

## Admin Patterns

### Separate Admin Controllers

A clean way to organize admin functionality is to create a separate `Admin` application (or namespace) with its own controllers. This keeps admin logic separate from public-facing code.

The typical project structure looks like this:

```
Web/
    Controller/
        Posts.hs        -- Public-facing controller
    View/
        Posts/
            Index.hs    -- Public views

Admin/
    Controller/
        Posts.hs        -- Admin controller
    View/
        Posts/
            Index.hs    -- Admin views
```

You can generate an Admin application using the code generator. In your `Admin/Types.hs`, define the admin controllers:

```haskell
data PostsController
    = PostsAction
    | NewPostAction
    | CreatePostAction
    | EditPostAction { postId :: !(Id Post) }
    | UpdatePostAction { postId :: !(Id Post) }
    | DeletePostAction { postId :: !(Id Post) }
    deriving (Eq, Show, Data)
```

### Admin beforeAction Guard

In each admin controller, add a `beforeAction` that checks the user is both logged in and has the admin role:

```haskell
-- Admin/Controller/Posts.hs
module Admin.Controller.Posts where

import Admin.Controller.Prelude
import Admin.View.Posts.Index
import Admin.View.Posts.New
import Admin.View.Posts.Edit

instance Controller PostsController where
    beforeAction = do
        ensureIsUser
        accessDeniedUnless (currentUser.role == Admin)

    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action NewPostAction = do
        let post = newRecord @Post
        render NewView { .. }

    -- ... other actions
```

If you have many admin controllers, you can extract the check into a helper to avoid repeating yourself. Place this in `Application/Helper/Controller.hs`:

```haskell
-- Application/Helper/Controller.hs
module Application.Helper.Controller
    ( module Application.Helper.Controller
    ) where

import IHP.ControllerPrelude

ensureIsAdmin :: (?context :: ControllerContext, ?request :: Request) => IO ()
ensureIsAdmin = do
    ensureIsUser
    accessDeniedUnless (currentUser.role == Admin)
```

Then in your admin controllers you can simply write:

```haskell
instance Controller PostsController where
    beforeAction = ensureIsAdmin
```

Note: If your project already has an `Admin` user type with `ensureIsAdmin` from `IHP.LoginSupport.Helper.Controller`, you should name this helper differently (e.g., `ensureIsAdminRole`) to avoid a name collision.

### Admin-Only UI Elements in Views

In views, you can use `currentUserOrNothing` to conditionally show elements that only admins should see:

```haskell
-- Web/View/Posts/Show.hs
instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>

        {adminControls post}
    |]

adminControls :: (?context :: ControllerContext) => Post -> Html
adminControls post = case currentUserOrNothing of
    Just user | user.role == Admin -> [hsx|
        <div class="admin-controls">
            <a href={EditPostAction post.id} class="btn btn-secondary">Edit</a>
            <a href={DeletePostAction post.id} class="btn btn-danger js-delete js-delete-no-confirm">Delete</a>
        </div>
    |]
    _ -> mempty
```

The `currentUserOrNothing` function is available in views through the View Prelude. It returns `Nothing` when the user is not logged in, and `Just user` when they are. This lets you safely check the role without causing an error for anonymous visitors.

You can also show different navigation items in your layout:

```haskell
-- Web/View/Layout.hs
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
    <!DOCTYPE html>
    <html>
        <body>
            <nav>
                {when isAdmin [hsx|<a href={AdminPostsAction}>Admin</a>|]}
            </nav>
            {inner}
        </body>
    </html>
|]
    where
        isAdmin = case currentUserOrNothing of
            Just user -> user.role == Admin
            Nothing -> False
```

Keep in mind that hiding UI elements is not a substitute for server-side authorization. Always check permissions in your controller actions as well, because a user could craft a request directly without going through the UI.

## Authorization Helpers

As your application grows, you will often check the same permission in multiple places -- in controllers to guard actions, and in views to show or hide UI elements. Instead of repeating the logic, define reusable helper functions.

### Defining Permission Helpers

Place your authorization helpers in `Application/Helper/Controller.hs` so they are available in all controllers:

```haskell
-- Application/Helper/Controller.hs
module Application.Helper.Controller
    ( module Application.Helper.Controller
    ) where

import IHP.ControllerPrelude

-- | Returns True if the given user is the author of the post
canEditPost :: (?context :: ControllerContext, ?request :: Request) => Post -> Bool
canEditPost post = post.userId == currentUserId

-- | Returns True if the current user has the admin role
isAdmin :: (?context :: ControllerContext, ?request :: Request) => Bool
isAdmin = currentUser.role == Admin

-- | Returns True if the current user can delete a post.
-- Admins can delete any post; regular users can only delete their own.
canDeletePost :: (?context :: ControllerContext, ?request :: Request) => Post -> Bool
canDeletePost post = isAdmin || canEditPost post
```

### Using Helpers in Controllers

```haskell
instance Controller PostsController where
    beforeAction = ensureIsUser

    action EditPostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (canEditPost post)
        render EditView { .. }

    action DeletePostAction { postId } = do
        post <- fetch postId
        accessDeniedUnless (canDeletePost post)
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction
```

### Using Helpers in Views

To use the same helpers in views, re-export them from `Application/Helper/View.hs`:

```haskell
-- Application/Helper/View.hs
module Application.Helper.View
    ( module Application.Helper.View
    , canEditPost
    , canDeletePost
    , isAdmin
    ) where

import IHP.ViewPrelude
import Application.Helper.Controller (canEditPost, canDeletePost, isAdmin)
```

Note that `currentUser` will redirect to the login page if no user is logged in. In views, triggering a redirect is not appropriate — a view should only produce HTML. For view helpers where the user might not be logged in, use `currentUserOrNothing` instead, which returns `Nothing` rather than redirecting. If your controller already verified authentication with `ensureIsUser` in `beforeAction`, then `currentUser` is safe to use in views too since you know a user is always present.

An alternative approach is to define separate view-safe helpers that use `currentUserOrNothing`:

```haskell
-- Application/Helper/View.hs
module Application.Helper.View
    ( module Application.Helper.View
    ) where

import IHP.ViewPrelude

-- | Check if the logged-in user can edit this post. Returns False if not logged in.
canEditPost :: (?context :: ControllerContext) => Post -> Bool
canEditPost post = case currentUserOrNothing of
    Just user -> post.userId == user.id
    Nothing -> False

-- | Check if the logged-in user is an admin. Returns False if not logged in.
isAdmin :: (?context :: ControllerContext) => Bool
isAdmin = case currentUserOrNothing of
    Just user -> user.role == Admin
    Nothing -> False

canDeletePost :: (?context :: ControllerContext) => Post -> Bool
canDeletePost post = isAdmin || canEditPost post
```

Then use them in your views:

```haskell
instance View ShowView where
    html ShowView { .. } = [hsx|
        <h1>{post.title}</h1>
        <p>{post.body}</p>

        {when (canEditPost post) editButton}
        {when (canDeletePost post) deleteButton}
    |]
        where
            editButton = [hsx|<a href={EditPostAction post.id} class="btn btn-secondary">Edit</a>|]
            deleteButton = [hsx|<a href={DeletePostAction post.id} class="btn btn-danger js-delete js-delete-no-confirm">Delete</a>|]
```

## Custom Error Pages

### Default Behavior

When `accessDeniedUnless` or `accessDeniedWhen` denies access, IHP returns an HTTP 403 response with a built-in "Access Denied" page. This default page shows the IHP logo and a simple "Error 403 - Access denied" message.

### Custom 403 Page

You can replace the default 403 page by creating a file at `static/403.html` in your project. When this file exists, IHP will serve it instead of the built-in page whenever access is denied.

For example, create `static/403.html`:

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1"/>
    <title>Access Denied</title>
    <style>
        body {
            font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif;
            display: flex;
            justify-content: center;
            align-items: center;
            min-height: 100vh;
            margin: 0;
            background-color: #f8f9fa;
        }
        .container { text-align: center; }
        h1 { color: #dc3545; }
    </style>
</head>
<body>
    <div class="container">
        <h1>403 - Access Denied</h1>
        <p>You do not have permission to view this page.</p>
        <a href="/">Go to Home Page</a>
    </div>
</body>
</html>
```

This static HTML file is served as-is. Since it is a plain HTML file (not an HSX template), you cannot use dynamic content like the current user's name. If you need dynamic content in your error page, consider catching the access denied case in your controller and rendering a custom view instead:

```haskell
    action EditPostAction { postId } = do
        post <- fetch postId
        unless (post.userId == currentUserId) do
            setErrorMessage "You do not have permission to edit this post"
            redirectTo ShowPostAction { postId }
        render EditView { .. }
```

This approach gives you full control over what the user sees and where they are redirected, while still preventing unauthorized access.

## Row-Level Security

For an additional layer of protection, IHP supports PostgreSQL Row-Level Security (RLS). With RLS, the database itself enforces that users can only access rows they are authorized to see, regardless of what your application code does.

See the [IHP DataSync documentation](https://ihp.digitallyinduced.com/Guide/realtime-spas.html) for details on how to set up RLS policies. With `AuthMiddleware (authMiddleware @User)` enabled in `Config.hs`, call `enableRowLevelSecurityIfLoggedIn` from your `FrontController.hs`:

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        enableRowLevelSecurityIfLoggedIn
```

With this enabled and appropriate policies defined in your schema, queries like `query @Post |> fetch` will automatically be filtered by the database to only return rows the current user is allowed to see.
