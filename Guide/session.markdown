# Session

```toc
```

## Introduction

The session provides a way for your application to store small amounts of information that will be persisted between requests. It's mainly used from inside your controller actions.

In general you should not store complex data structures in the session. It's better to store scalar values in there only. For example: Store the current user id instead of the current user record.

The session works by storing the data inside a cryptographically signed and encrypted cookie on the client. The encryption key is generated automatically and is stored at `Config/client_session_key.aes`. Internally IHP uses the [clientsession](https://hackage.haskell.org/package/clientsession-0.9.1.2/docs/Web-ClientSession.html) library. You can find more technical details on the implementation in the clientsession documentation.

## Accessing the Session

### Writing

In your controller action, use `setSession` to store a value:

```haskell
action SessionExampleAction = do
    setSession "userEmail" "hi@digitallyinduced.com"
```

Right now, `setSession` only accepts `Text` values. Other types like `Int` have to be converted to `Text` using `tshow theIntValue`.

### Reading

You can use `getSession` to retrieve a value:

```haskell
action SessionExampleAction = do
    userEmail <- getSession "userEmail"
```

`userEmail` is set to `Just "hi@digitallyinduced.com"` when the value has been set before. Otherwise it will be `Nothing`.

For convience you can use `getSessionInt` to retrieve the value as a `Maybe Int`, and `getSessionUUID` to retrieve the value as a `Maybe UUID`:

```haskell
action SessionExampleAction = do
    counter :: Maybe Int <- getSessionInt "counter"
    userId :: Maybe UUID <- getSessionUUID "userId"
```

### Deleting

Set a value to an empty Text to remove it from the session:

```haskell
action LogoutAction = do
    setSession "userId" ""
```

## Flash Messages

Flash Messages provide a way to show success or error messages on the next request. After setting a success or error message, the message is only available on the next view rendered by the browser.

In the following view you can see a success flash message `Post created`:

![Example Flash Message](images/first-project/index_view.png).

### Setting a Message

Use `setSuccessMessage` to set a success message:

```haskell
action CreatePostAction = do
    ...
    setSuccessMessage "Your Post has been created succesfully"
    redirectTo ShowPostAction { .. }
```

Use `setErrorMessage` to set a failure message:

```haskell
action CreatePostAction = do
    unless isAllowedToPost do
        setErrorMessage "You don't have the required permissions to create a post"
        redirectTo NewPostAction
    
    ...
```

In both cases the messages are stored inside the session. The message value is automatically cleared after the next request (except redirects or when sending a JSON response).

### Rendering a Message

You have to make sure that `{renderFlashMessages}` is displayed somewhere in your layout or view, otherwise the flash message is not visible.

Here is an example of a layout calling `renderFlashMessages`:

```haskell
defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "de" $ [hsx|
<head></head>
<body>
    <div class="container mt-2">
        {renderFlashMessages}
        {inner}
    </div>
</body>
|]
```

The rendered HTML looks like this:

```html
<div class="alert alert-success">{successMessage}</div>
<div class="alert alert-danger">{errorMessage}</div>
```

To display the Flash Messages in custom way, you can always access them using `viewContext |> #flashMessages` in your views. This returns a list of `FlashMessage`. You can also take a look at the [`renderFlashMessages`](https://ihp.digitallyinduced.com/api-docs/IHP-ViewSupport.html#v:renderFlashMessages) implementation and copy the code into your view, and then make customizations.

## Session Cookie

The cookie max age is set to 30 days by default. To protect against CSRF, the SameSite Policy is set to Lax.

