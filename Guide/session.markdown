# Session

```toc

```

## Introduction

The session provides a way for your application to store small amounts of information that will be persisted between requests. It's mainly used from inside your controller actions.

In general, you should not store complex data structures in the session. It's better to store scalar values in there only. For example: Store the current user-id instead of the current user record.

The session works by storing the data inside a cryptographically signed and encrypted cookie on the client. The encryption key is generated automatically and is stored at `Config/client_session_key.aes`. Internally IHP uses the [clientsession](https://hackage.haskell.org/package/clientsession-0.9.1.2/docs/Web-ClientSession.html) library. You can find more technical details on the implementation in the clientsession documentation.

## Accessing the Session

### Writing

In your controller action, use [`setSession`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Session.html#v:setSession) to store a value:

```haskell
action SessionExampleAction = do
    setSession "userEmail" "hi@digitallyinduced.com"
```

You can use [`setSession`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Session.html#v:setSession) with other data types like `Int`, `Bool` or `UUID` as well:


```haskell
action SessionExampleAction = do
    let meaningOfLife :: Int = 42
    setSession "meaningOfLife" meaningOfLife
```

### Reading

You can use [`getSession`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Session.html#v:getSession) to retrieve a value:

```haskell
action SessionExampleAction = do
    userEmail :: Maybe Text <- getSession "userEmail"
```

`userEmail` is set to `Just "hi@digitallyinduced.com"` when the value has been set before. Otherwise, it will be `Nothing`.

The [`getSession`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Session.html#v:getSession) also supports other data types like `Int`, `Bool` or `UUID`:

```haskell
action SessionExampleAction = do
    counter :: Maybe Int <- getSession "counter"
    userId :: Maybe UUID <- getSession "userId"
```

### Deleting

Use [`deleteSession`](https://ihp.digitallyinduced.com/api-docs/IHP-Controller-Session.html#v:deleteSession) to remove a value from the session:

```haskell
action LogoutAction = do
    deleteSession "userId"
```

## Flash Messages

Flash Messages provide a way to show success or error messages on the next request. After setting a success or error message, the message is only available on the next view rendered by the browser.

In the following view you can see a success flash message `Post created`:

![Example Flash Message](images/first-project/index_view.png).

### Setting a Message

Use [`setSuccessMessage`](https://ihp.digitallyinduced.com/api-docs/IHP-FlashMessages-ControllerFunctions.html#v:setSuccessMessage) to set a success message:

```haskell
action CreatePostAction = do
    ...
    setSuccessMessage "Your Post has been created succesfully"
    redirectTo ShowPostAction { .. }
```

Use [`setErrorMessage`](https://ihp.digitallyinduced.com/api-docs/IHP-FlashMessages-ControllerFunctions.html#v:setErrorMessage) to set a failure message:

```haskell
action CreatePostAction = do
    unless isAllowedToPost do
        setErrorMessage "You don't have the required permissions to create a post"
        redirectTo NewPostAction

    ...
```

In both cases, the messages are stored inside the session. The message value is automatically cleared after the next request (except redirects or when sending a JSON response).

### Rendering a Message

You have to make sure that [`{renderFlashMessages}`](https://ihp.digitallyinduced.com/api-docs/IHP-FlashMessages-ViewFunctions.html#v:renderFlashMessages) is displayed somewhere in your layout or view, otherwise, the flash message is not visible.

Here is an example of a layout calling [`renderFlashMessages`](https://ihp.digitallyinduced.com/api-docs/IHP-FlashMessages-ViewFunctions.html#v:renderFlashMessages):

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

To display the Flash Messages in a custom way, you can always access them using `let flashMessages :: [FlashMessage] = fromFrozenContext` in your views. This returns a list of [`FlashMessage`](https://ihp.digitallyinduced.com/api-docs/IHP-FlashMessages-Types.html#t:FlashMessage). You can also take a look at the [`renderFlashMessages`](https://ihp.digitallyinduced.com/api-docs/IHP-FlashMessages-ViewFunctions.html#v:renderFlashMessages) implementation and copy the code into your view, and then make customizations.

## Session Cookie

The cookie max-age is set to 30 days by default. To protect against CSRF, the SameSite Policy is set to Lax.
