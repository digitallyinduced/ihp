# Flash Messages

```toc

```

## Introduction

Flash messages are short notifications displayed to the user after an action, such as "Post created successfully" or "You don't have permission to do that." They appear once and are automatically cleared after the next page load.

IHP has built-in support for two types of flash messages:

- **Success messages** -- shown in a green alert (Bootstrap) or a green-styled div (Tailwind)
- **Error messages** -- shown in a red alert (Bootstrap) or a red-styled div (Tailwind)

Flash messages are stored in the session cookie. When you set a flash message in a controller action, it persists through a redirect and is displayed on the next page the user sees. After that page renders, the message is automatically removed.

If you generated your project with IHP, flash messages already work out of the box. The default layout includes `{renderFlashMessages}`, and the controller prelude exports `setSuccessMessage` and `setErrorMessage`.

## Setting Flash Messages

You can set flash messages from any controller action using `setSuccessMessage` and `setErrorMessage`.

### Success Messages

Use `setSuccessMessage` to notify the user that an operation completed successfully:

```haskell
action CreatePostAction = do
    let post = newRecord @Post
    post
        |> buildPost
        |> ifValid \case
            Left post -> render NewView { .. }
            Right post -> do
                post <- post |> createRecord
                setSuccessMessage "Post created"
                redirectTo PostsAction
```

### Error Messages

Use `setErrorMessage` to notify the user that something went wrong:

```haskell
action CreateTeamAction = do
    unless userOnPaidPlan do
        setErrorMessage "This requires you to be on the paid plan"
        redirectTo NewTeamAction

    ...
```

You can pass any `Text` value as the message:

```haskell
setSuccessMessage "Your changes have been saved"
setErrorMessage "Something went wrong, please try again"
```

## Displaying Flash Messages

Flash messages are rendered by calling `renderFlashMessages` inside your layout. If you generated your project with IHP, this is already set up for you in your `Web/View/Layout.hs`.

### Default Layout

The generated layout looks something like this:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = [hsx|
<!DOCTYPE html>
<html lang="en">
    <head></head>
    <body>
        <div class="container mt-4">
            {renderFlashMessages}
            {inner}
        </div>
    </body>
</html>
|]
```

`renderFlashMessages` checks for any flash messages on the current request and renders them as HTML. If there are no flash messages, it renders nothing.

### Rendered HTML

When using the default Bootstrap CSS framework, the rendered HTML looks like this:

For a success message:
```html
<div class="alert alert-success">Post created</div>
```

For an error message:
```html
<div class="alert alert-danger">Something went wrong</div>
```

When using Tailwind CSS, the rendered HTML uses Tailwind utility classes instead:

```html
<div class="bg-green-100 border border-green-500 text-green-900 px-4 py-3 rounded relative">Post created</div>
```

## Example: Using Flash Messages in a Controller

Here is a complete controller that uses flash messages for create, update, and delete actions:

```haskell
module Web.Controller.Posts where

import Web.Controller.Prelude

instance Controller PostsController where
    action PostsAction = do
        posts <- query @Post |> fetch
        render IndexView { .. }

    action NewPostAction = do
        let post = newRecord
        render NewView { .. }

    action CreatePostAction = do
        let post = newRecord @Post
        post
            |> buildPost
            |> ifValid \case
                Left post -> render NewView { .. }
                Right post -> do
                    post <- post |> createRecord
                    setSuccessMessage "Post created"
                    redirectTo PostsAction

    action EditPostAction { postId } = do
        post <- fetch postId
        render EditView { .. }

    action UpdatePostAction { postId } = do
        post <- fetch postId
        post
            |> buildPost
            |> ifValid \case
                Left post -> render EditView { .. }
                Right post -> do
                    post <- post |> updateRecord
                    setSuccessMessage "Post updated"
                    redirectTo EditPostAction { .. }

    action DeletePostAction { postId } = do
        post <- fetch postId
        deleteRecord post
        setSuccessMessage "Post deleted"
        redirectTo PostsAction

buildPost post = post
    |> fill @["title", "body"]
```

When validation fails (the `Left` branch), no flash message is set. The form is re-rendered with validation errors displayed inline. When the action succeeds (the `Right` branch), a success message is set and the user is redirected.

## Customizing Flash Message Appearance

### CSS Styling

Flash messages use Bootstrap's [alert component](https://getbootstrap.com/docs/5.3/components/alerts/) by default. You can customize their appearance with CSS:

```css
/* Make success alerts stand out more */
.alert-success {
    border-left: 4px solid #198754;
}

/* Make error alerts stand out more */
.alert-danger {
    border-left: 4px solid #dc3545;
}
```

### Custom HTML Structure

If you want full control over the flash message HTML, you can customize the CSS framework's `styledFlashMessage` function. In your `Web/View/Layout.hs` or `Config/Config.hs`, override the default:

```haskell
import IHP.View.CSSFramework.Bootstrap (bootstrap)
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh

customFramework :: CSSFramework
customFramework = bootstrap
    { styledFlashMessage = weightedFlashMessage
    }
    where
        weightedFlashMessage _ (SuccessFlashMessage message) = [hsx|
            <div class="alert alert-success d-flex align-items-center" role="alert">
                <strong>Success:</strong>&nbsp;{message}
            </div>
        |]
        weightedFlashMessage _ (ErrorFlashMessage message) = [hsx|
            <div class="alert alert-danger d-flex align-items-center" role="alert">
                <strong>Error:</strong>&nbsp;{message}
            </div>
        |]
```

Then set the custom framework in your `Config/Config.hs`:

```haskell
config :: ConfigBuilder
config = do
    option $ AppHostname "localhost"
    option customFramework
```

### Accessing Flash Messages Directly

If you want to render flash messages in a completely custom way without using the CSS framework, you can access the raw flash message list in your views using `theFlashMessages`:

```haskell
myLayout :: Html -> Html
myLayout inner = [hsx|
<html>
    <body>
        {forEach theFlashMessages renderMessage}
        {inner}
    </body>
</html>
|]
    where
        renderMessage (SuccessFlashMessage message) = [hsx|
            <div class="my-custom-success">{message}</div>
        |]
        renderMessage (ErrorFlashMessage message) = [hsx|
            <div class="my-custom-error">{message}</div>
        |]
```

You will need to import the `FlashMessage` type:

```haskell
import Network.Wai.Middleware.FlashMessages (FlashMessage (..))
```

## Flash Messages and Redirects

Flash messages are designed to work across redirects. The typical pattern is:

1. Set a flash message in a controller action.
2. Redirect the user to another page.
3. The flash message is displayed on that page.
4. The flash message is automatically cleared after the page renders.

```haskell
action DeletePostAction { postId } = do
    post <- fetch postId
    deleteRecord post
    setSuccessMessage "Post deleted"  -- Step 1: Set the message
    redirectTo PostsAction             -- Step 2: Redirect
    -- Step 3: PostsAction renders the page, and "Post deleted" is shown
    -- Step 4: On the next request, the message is gone
```

This works because flash messages are stored in the session cookie. The `setSuccessMessage` call writes the message to the session. When the browser follows the redirect, the message is still in the session. IHP's middleware reads the message from the session, makes it available for rendering, and clears it from the session -- all in the same request.

If you set a flash message but render a view directly (without redirecting), the message will be shown on the *next* page load instead, not the current one. This is usually not what you want. The common pattern is to always redirect after setting a flash message.
