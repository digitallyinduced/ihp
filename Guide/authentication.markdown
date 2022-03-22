# Authentication

```toc

```

## Quick-and-Dirty: HTTP Basic Auth

While IHP provides an authentication toolkit out of the box, it also provides a shortcut for cases where you just want the simplest possible way to enforce a hard-coded username/password before accessing your new web application. This shortcut leverages HTTP Basic Authentication built into all browsers:

```haskell
instance Controller WidgetsController where
    beforeAction = basicAuth "sanja" "hunter2" "myapp"
```

The parameters are: username, password, and authentication realm. The realm can be thought of as an area of validity for the credentials. It is common to put the project name, but it can also be blank (meaning the entire domain).

## Introduction - Real Authentication

--

**[There's an IHP Casts Episode on this part of the Documentation](https://www.youtube.com/watch?v=vNaJZuVoeuc&list=PLenFm8BWuKlS0IaE31DmKB_PbkMLmwWmG&index=12)**

[![](https://i.ytimg.com/vi/vNaJZuVoeuc/maxresdefault.jpg)](https://www.youtube.com/watch?v=vNaJZuVoeuc&list=PLenFm8BWuKlS0IaE31DmKB_PbkMLmwWmG&index=12)

--

The usual convention in IHP is to call your user record `User`. When there is an admin user, we usually call the record `Admin`. In general, the authentication can work with any kind of record. The only requirement is that it has an id field.

To use the authentication module, your `users` table needs to have at least an `id`, `email`, `password_hash`, `locked_at` and `failed_login_attempts` field. Add this to `Schema.sql`:

```sql
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
```

The password authentication saves the passwords as a salted hash using the [pwstore-fast library](https://hackage.haskell.org/package/pwstore-fast-2.4.4/docs/Crypto-PasswordStore.html). By default, a user will be locked for an hour after 10 failed login attempts.

## Setup

Currently, the authentication toolkit has to be enabled manually. We plan to do this setup using a code generator in the future.

#### Adding a Session Controller

We need to add a new controller that will deal with the login and logout. We call this the `SessionsController`.

First, we have to update `Web/Types.hs`:

```haskell
import IHP.LoginSupport.Types -- <---- ADD THIS IMPORT

-- ADD THIS TO THE END OF THE FILE

instance HasNewSessionUrl User where
    newSessionUrl _ = "/NewSession"

type instance CurrentUserRecord = User
```

The `instance HasNewSessionUrl User` tells the auth module where to redirect a user in case the user tries to access a action that requires login. The definition of [`CurrentUserRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Types.html#t:CurrentUserRecord) tells the auth system to use our `User` type within the login system.

We also need to add the type definitions for the `SessionsController`:

```haskell
data SessionsController
    = NewSessionAction
    | CreateSessionAction
    | DeleteSessionAction
    deriving (Eq, Show, Data)
```

After that we need to set up routing for our new controller in `Web/Routes.hs`:

```haskell
instance AutoRoute SessionsController
```

We also need to create a `Web/Controller/Sessions.hs` calling the IHP authentication module:

```haskell
module Web.Controller.Sessions where

import Web.Controller.Prelude
import Web.View.Sessions.New
import qualified IHP.AuthSupport.Controller.Sessions as Sessions

instance Controller SessionsController where
    action NewSessionAction = Sessions.newSessionAction @User
    action CreateSessionAction = Sessions.createSessionAction @User
    action DeleteSessionAction = Sessions.deleteSessionAction @User

instance Sessions.SessionsControllerConfig User
```

Additionally, we need to implement a login view at `Web/View/Sessions/New.hs` like this:

```haskell
module Web.View.Sessions.New where
import Web.View.Prelude
import IHP.AuthSupport.View.Sessions.New

instance View (NewView User) where
    html NewView { .. } = [hsx|
        <div class="h-100" id="sessions-new">
            <div class="d-flex align-items-center">
                <div class="w-100">
                    <div style="max-width: 400px" class="mx-auto mb-5">
                        <h5>Please login</h5>
                        {renderForm user}
                    </div>
                </div>
            </div>
        </div>
    |]

renderForm :: User -> Html
renderForm user = [hsx|
    <form method="POST" action={CreateSessionAction}>
        <div class="form-group">
            <input name="email" value={get #email user} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" />
        </div>
        <div class="form-group">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary btn-block">Login</button>
    </form>
|]
```


#### Activating the Session

Open `Web/FrontController.hs`. Add an import for `IHP.LoginSupport.Middleware` and `Web.Controller.Sessions`:

```haskell
import IHP.LoginSupport.Middleware
import Web.Controller.Sessions
```

We then need to mount our session controller by adding [`parseRoute @SessionController`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseRoute):

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @SessionsController -- <--------------- add this
        -- Generator Marker
        ]
```

At the end of the file, there is a line like:

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
```

We need to extend this function with a [`initAuthentication @User`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Middleware.html#v:initAuthentication) like this:

```haskell
instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initAuthentication @User
```

This will fetch the user from the database when a `userId` is given in the session. The fetched user record is saved to the special `?context` variable and is used by all the helper functions like [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser).


## Trying out the login

After you have completed the above steps, you can open the login at `/NewSession`. You can generate a link to your login page like this:

```html
<a href={NewSessionAction}>Login</a>
```

## Accessing the current user

Inside your actions you can then use [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) to get access to the current logged in user:

```haskell
action MyAction = do
    let text = "Hello " <> (get #email currentUser)
    renderPlain text
```

In case the user is logged out, an exception will be thrown when accessing [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) and the browser will automatically be redirected to the `NewSessionAction`.

You can use [`currentUserOrNothing`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUserOrNothing) to manually deal with the not-logged-in case:

```haskell
action MyAction = do
    case currentUserOrNothing of
        Just currentUser -> do
            let text = "Hello " <> (get #email currentUser)
            renderPlain text
        Nothing -> renderPlain "Please login first"
```

Additionally you can use [`currentUserId`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUserId) as a shortcut for `currentUser |> get #id`.

You can also access the user using [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) inside your views:

```html
[hsx|
<h1>Hello {get #email currentUser}</h1>
|]
```

## Performing actions on login

The sessioncontroller has a convenient [`beforeLogin`](https://ihp.digitallyinduced.com/api-docs/IHP-AuthSupport-Controller-Sessions.html#v:beforeLogin) which is run on login after the user is authenticated, but before the target page is rendered. This can be useful for updating last login time, number of logins or aborting the login when the user is blocked. Add code for it in your `Web/Controller/Sessions.hs`. To update number of logins (requires `logins` integer field in `Users` table):

```haskell
instance Sessions.SessionsControllerConfig User where
    beforeLogin = updateLoginHistory

updateLoginHistory user = do
    user
        |> modify #logins (\count -> count + 1)
        |> updateRecord
    pure ()
```

To block login (requires `isConfirmed` boolean field in `Users` table):

```haskell
instance Sessions.SessionsControllerConfig User where
    beforeLogin user = do
        unless (get #isConfirmed user) do
            setErrorMessage "Please click the confirmation link we sent to your email before you can use IHP Cloud"
            redirectTo NewSessionAction
```

## Logout

You can simply render a link inside your layout or view to send the user to the logout:

```haskell
<a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
```

## Creating a user

To create a user with a hashed password, you just need to call the hashing function before saving it into the database:

```haskell
    action CreateUserAction = do
        let user = newRecord @User
        user
            |> fill @["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
                    redirectToPath "/"
```

### Hashing a Password

To manually insert a user into your database you need to hash the password first. You can do this by calling the `hash-password` tool from your command line:

```bash
$ hash-password
Enter your password and press enter:
hunter2
sha256|17|Y32Ga1uke5CisJvVp6p2sg==|TSDuEs1+Xdaels6TYCkyCgIBHxWA/US7bvBlK0vHzvc=
```

Use [`hashPassword`](https://ihp.digitallyinduced.com/api-docs/IHP-AuthSupport-Authentication.html#v:hashPassword) to hash a password from inside your application.

### Email Confirmation

*Requires IHP Pro*

In production apps you typically want to send a confirmation email to the user before the user can log in.

To enable email confirmation add a `confirmation_token` and `is_confirmed` column to your `users` table:

```sql
CREATE TABLE users (
    /* ... */

    confirmation_token TEXT DEFAULT NULL,
    is_confirmed BOOLEAN DEFAULT false NOT NULL
);
```

#### Confirmation Action

First we need to create a new confirmation action to our `UsersController`.

Open `Web/Types.hs` and add a `| ConfirmUserAction { userId :: !(Id User), confirmationToken :: !Text }` to the `data UsersController`:

```haskell
data UsersController
    = NewUserAction
    -- ...
    | ConfirmUserAction { userId :: !(Id User), confirmationToken :: !Text } -- <--- ADD THIS ACTION
    deriving (Eq, Show, Data)
```

Next open `Web/Controller/Users.hs` and:

1. Add these imports to the top of the file:
    ```haskell
    import qualified IHP.AuthSupport.Controller.Confirmations as Confirmations
    import qualified Web.Controller.Sessions ()
    ```
2. Add this action implementation:
    ```haskell
    action ConfirmUserAction { userId, confirmationToken } = Confirmations.confirmAction userId confirmationToken
    ```
    This will delegate all calls of the confirm action to the IHP confirmation action.
3. Add an instance of `Confirmations.ConfirmationsControllerConfig User`:
    ```haskell
    instance Confirmations.ConfirmationsControllerConfig User where
    ```
    This instance can be used to customize the confirmation process. E.g. to send an welcome email after confirmation. For now we don't customize anything here yet, therefore we leave the instance empty.

#### Confirmation Mail

Next we need to send out the confirmation mail.

Create a new file at `Web/Mail/Users/ConfirmationMail.hs` and copy paste the following template in there:

```haskell
module Web.Mail.Users.ConfirmationMail where
import Web.View.Prelude
import IHP.MailPrelude
import IHP.AuthSupport.Confirm

instance BuildMail (ConfirmationMail User) where
    subject = "Confirm your Account"
    to ConfirmationMail { .. } = Address { addressName = Nothing, addressEmail = get #email user }
    from = "someone@example.com"
    html ConfirmationMail { .. } = [hsx|
        Hey,
        just checking it's you.

        <a href={urlTo (ConfirmUserAction (get #id user) confirmationToken)} target="_blank">
            Activate your Account
        </a>
    |]
```

[You can change this email to your liking.](mail.html)

#### Sending the Confirmation Mail

To send out the confirmation mail, open your registration action. Typically this is the `CreateUserAction` in `Web/Controller/Users.hs`.

1. Add imports
    ```haskell
    import IHP.AuthSupport.Confirm
    import Web.Mail.Users.ConfirmationMail
    ```
2. Call `sendConfirmationMail user` after the user has been created in `action CreateUserAction`:
    ```haskell
    action CreateUserAction = do
        let user = newRecord @User
        user
            |> fill @["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword (get #passwordHash user)
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord


                    sendConfirmationMail user -- <------ ADD THIS FUNCTION CALL TO YOUR ACTION


                    -- We can also customize the flash message text to let the user know that we have sent him an email
                    setSuccessMessage $ "Welcome onboard! Before you can start, please quickly confirm your email address by clicking the link we've sent to " <> get #email user
                    
                    redirectTo NewSessionAction
    ```

Now whenever a user registers, he will receive our confirmation mail.

#### Disallowing Login for unconfirmed Users

We still need to ensure that a user cannot log in before the email is confirmed.

Open `Web/Controller/Sessions.hs` and:

1. Add an import to `IHP.AuthSupport.Confirm` at the top of the file:
    ```haskell
    import qualified IHP.AuthSupport.Confirm as Confirm
    ```
2. Append a call to `Confirm.ensureIsConfirmed user` to the `beforeLogin` function of `SessionsControllerConfig`:

    ```haskell
    instance Sessions.SessionsControllerConfig User where
        beforeLogin user = do
            Confirm.ensureIsConfirmed user
    ```

Now all logins by users that are not confirmed are blocked.

#### Optional: Send a Welcome Email after Confirmation (After-Confirmation Hook)

You can use the `ConfirmationsControllerConfig` instance defined in `Web/Controller/Users.hs` to run any code after the user is confirmed:

```haskell
-- Web/Controller/Users.hs

instance Confirmations.ConfirmationsControllerConfig User where
    afterConfirmation user = do
        -- This code here is called whenever a user was confirmed
```


A common scenario is to send a welcome email after the user is confirmed. Let's asume you've already created a new `WelcomeEmail` using the Email Code Generator. You can then use this to send the welcome email after confirmation:

```haskell
instance Confirmations.ConfirmationsControllerConfig User where
    afterConfirmation user = do
        sendMail WelcomeMail { user }
```


## Aside: Admin authentication

If you are creating an admin sub-application, first use the code generator to create an application called `Admin`, then follow this guide replacing `Web` with `Admin` and `User` with `Admin` everywhere (except for the lower-case `user` in the file `Admin/View/Sessions/New.hs`, which comes from an imported module).


[Next: Authorization](https://ihp.digitallyinduced.com/Guide/authorization.html)
