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

The `instance HasNewSessionUrl User` tells the auth module where to redirect a user in case the user tries to access an action that requires login. The definition of [`CurrentUserRecord`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Types.html#t:CurrentUserRecord) tells the auth system to use our `User` type within the login system.

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
        <div class="mb-3">
            <input name="email" value={user.email} type="email" class="form-control" placeholder="E-Mail" required="required" autofocus="autofocus" />
        </div>
        <div class="mb-3">
            <input name="password" type="password" class="form-control" placeholder="Password"/>
        </div>
        <button type="submit" class="btn btn-primary w-100">Login</button>
    </form>
|]
```


#### Activating the Session

First, open `Config/Config.hs` and add the authentication middleware. This middleware runs on every request, fetches the user from the database when a session is active, and stores the user in the WAI request vault:

```haskell
import IHP.LoginSupport.Middleware

config :: ConfigBuilder
config = do
    option $ AuthMiddleware (authMiddleware @User)
```

Next, open `Web/FrontController.hs`. Add an import for `Web.Controller.Sessions`:

```haskell
import Web.Controller.Sessions
```

We then need to mount our session controller by adding [`parseRoute @SessionsController`](https://ihp.digitallyinduced.com/api-docs/IHP-RouterSupport.html#v:parseRoute):

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @SessionsController -- <--------------- add this
        -- Generator Marker
        ]
```

The authenticated user is available via helper functions like [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) in your controllers and views.


## Trying out the login

After you have completed the above steps, you can open the login at `/NewSession`. You can generate a link to your login page like this:

```html
<a href={NewSessionAction}>Login</a>
```

## Accessing the current user

Inside your actions you can then use [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) to get access to the current logged in user:

```haskell
action MyAction = do
    let text = "Hello " <> currentUser.email
    renderPlain text
```

In case the user is logged out, an exception will be thrown when accessing [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) and the browser will automatically be redirected to the `NewSessionAction`.

You can use [`currentUserOrNothing`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUserOrNothing) to manually deal with the not-logged-in case:

```haskell
action MyAction = do
    case currentUserOrNothing of
        Just currentUser -> do
            let text = "Hello " <> currentUser.email
            renderPlain text
        Nothing -> renderPlain "Please login first"
```

Additionally you can use [`currentUserId`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUserId) as a shortcut for `currentUser.id`.

You can also access the user using [`currentUser`](https://ihp.digitallyinduced.com/api-docs/IHP-LoginSupport-Helper-Controller.html#v:currentUser) inside your views:

```html
[hsx|
<h1>Hello {currentUser.email}</h1>
|]
```

## Performing actions on login

The `SessionsController` has a convenient [`beforeLogin`](https://ihp.digitallyinduced.com/api-docs/IHP-AuthSupport-Controller-Sessions.html#v:beforeLogin) which is run on login after the user is authenticated, but before the target page is rendered. This can be useful for updating last login time, number of logins or aborting the login when the user is blocked. Add code for it in your `Web/Controller/Sessions.hs`. To update number of logins (requires `logins` integer field in `Users` table):

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
        unless user.isConfirmed do
            setErrorMessage "Please click the confirmation link we sent to your email before you can use the App"
            redirectTo NewSessionAction
```

## Logout

You can simply render a link inside your layout or view to send the user to the logout:

```haskell
<a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>
```

## Creating a user

We have the login now, but we still need be able to register a user. On the IDE, right click over the `users` table and select "Generate Controller".

Creating a user is similar to creating any other record. However, one notable difference is that we need to hash the password. We can do that by calling the hashing function before saving it into the database:

```haskell
-- Web/Controller/Users.hs
    action CreateUserAction = do
        let user = newRecord @User
        -- The value from the password confirmation input field.
        let passwordConfirmation = param @Text "passwordConfirmation"
        user
            |> fill @'["email", "passwordHash"]
            -- We ensure that the error message doesn't include
            -- the entered password.
            |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
            |> validateField #passwordHash nonEmpty
            |> validateField #email isEmail
            -- After this validation, since it's operation on the IO, we'll need to use >>=.
            |> validateIsUnique #email
            >>= ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword user.passwordHash
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord
                    setSuccessMessage "You have registered successfully"
                    redirectToPath "/"
```

The view would look like this, removing the `failedLoginAttempts` that was generated by the IDE, and manually add a password confirmation input field.

```haskell
-- Web/View/Users/New.hs
renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(passwordField #passwordHash) {fieldLabel = "Password", required = True}}

    {(passwordField #passwordHash) { required = True, fieldLabel = "Password confirmation", fieldName = "passwordConfirmation", validatorResult = Nothing }}

    {submitButton}
|]
```

You'll notice we have two passwordFields. The first one is the password field, and the second one is the password confirmation field. We need to add a `fieldName` to the second one, so that it will be submitted as `passwordConfirmation` instead of `passwordHash`. We also need to set the `validatorResult` to `Nothing`, so that the validation error message doesn't show up twice.

We could have hand written `<input type="password" name="passwordConfirmation" required/>`, however we'd like the theming of the fields to be consistent.


### Editing a User

When editing an existing user we need to special case the password handling. A user may edit their info, but
without changing their password. In this case we don't want to re-hash the empty input.
Furthermore, we want to make sure that when we present the form, we don't populate the password field with the hashed password!

```haskell
-- Web/Controller/Users.hs
    action UpdateUserAction { userId } = do
        user <- fetch userId
        let originalPasswordHash = user.passwordHash
        -- The value from the password confirmation input field.
        let passwordConfirmation = param @Text "passwordConfirmation"
        user
            |> fill @'["email", "passwordHash"]
            -- We only validate the email field isn't empty, as the password
            -- can remain empty. We ensure that the error message doesn't include
            -- the entered password.
            |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
            |> validateField #passwordHash nonEmpty
            |> validateField #email isEmail
            -- After this validation, since it's operation on the IO, we'll need to use >>=.
            |> validateIsUnique #email
            >>= ifValid \case
                Left user -> render EditView { .. }
                Right user -> do
                    -- If the password hash is empty, then the user did not
                    -- change the password. So, we set the password hash to
                    -- the original password hash.
                    hashed <-
                        if user.passwordHash == ""
                            then pure originalPasswordHash
                            else hashPassword user.passwordHash

                    user <- user
                        |> set #passwordHash hashed
                        |> updateRecord
                    setSuccessMessage "User updated"
                    redirectTo EditUserAction { .. }
```

In the case of Editing, the password and password confirmation are optional. If however, the user has changed
the password, they will also need to confirm it.


```haskell
-- Web/View/Users/Edit.hs
renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email)}
    {(passwordField #passwordHash) {fieldLabel = "Password"}}

    {(passwordField #passwordHash) { fieldLabel = "Password confirmation", fieldName = "passwordConfirmation", validatorResult = Nothing }}
    {submitButton}
|]
```

### Hashing a Password via CLI

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
    to ConfirmationMail { .. } = Address { addressName = Nothing, addressEmail = user.email }
    from = "someone@example.com"
    html ConfirmationMail { .. } = [hsx|
        Hey,
        just checking it's you.

        <a href={urlTo (ConfirmUserAction user.id confirmationToken)} target="_blank">
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
            |> fill @'["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword user.passwordHash
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord


                    sendConfirmationMail user -- <------ ADD THIS FUNCTION CALL TO YOUR ACTION


                    -- We can also customize the flash message text to let the user know that we have sent him an email
                    setSuccessMessage $ "Welcome onboard! Before you can start, please quickly confirm your email address by clicking the link we've sent to " <> user.email

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


A common scenario is to send a welcome email after the user is confirmed. Let's assume you've already created a new `WelcomeMail` using the Email Code Generator. You can then use this to send the welcome email after confirmation:

```haskell
import IHP.Mail (sendMail)

instance Confirmations.ConfirmationsControllerConfig User where
    afterConfirmation user = do
        sendMail WelcomeMail { user }
```


## Aside: Admin authentication

If you are creating an admin sub-application, first use the code generator to create an application called `Admin`, then follow this guide replacing `Web` with `Admin` and `User` with `Admin` everywhere (except for the lower-case `user` in the file `Admin/View/Sessions/New.hs`, which comes from an imported module).

To enable both User and Admin authentication, compose the middleware in `Config/Config.hs`:

```haskell
option $ AuthMiddleware (authMiddleware @User . adminAuthMiddleware @Admin)
```


## User Registration

The existing "Creating a User" section above shows the core pattern. Here is a more complete registration flow that includes auto-login after registration.

### Registration Controller

Add a `NewUserAction` and `CreateUserAction` to your `UsersController` in `Web/Types.hs`:

```haskell
data UsersController
    = NewUserAction
    | CreateUserAction
    deriving (Eq, Show, Data)
```

Add routing in `Web/Routes.hs`:

```haskell
instance AutoRoute UsersController
```

Mount the controller in `Web/FrontController.hs`:

```haskell
import Web.Controller.Users

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        , parseRoute @SessionsController
        , parseRoute @UsersController -- <--- add this
        -- Generator Marker
        ]
```

Here is a complete `Web/Controller/Users.hs` with validation and auto-login:

```haskell
module Web.Controller.Users where

import Web.Controller.Prelude
import Web.View.Users.New

instance Controller UsersController where
    action NewUserAction = do
        let user = newRecord @User
        render NewView { .. }

    action CreateUserAction = do
        let user = newRecord @User
        let passwordConfirmation = param @Text "passwordConfirmation"
        user
            |> fill @'["email", "passwordHash"]
            |> validateField #email isEmail
            |> validateField #passwordHash nonEmpty
            |> validateField #passwordHash (hasMinLength 8)
            |> validateField #passwordHash (isEqual passwordConfirmation |> withCustomErrorMessage "Passwords don't match")
            |> validateIsUnique #email
            >>= ifValid \case
                Left user -> render NewView { .. }
                Right user -> do
                    hashed <- hashPassword user.passwordHash
                    user <- user
                        |> set #passwordHash hashed
                        |> createRecord

                    -- Auto-login after registration
                    login user

                    setSuccessMessage "Your account has been created"
                    redirectTo PostsAction -- Redirect to your app's main page
```

Note the validation pipeline:

- `isEmail` checks for a valid email format.
- `nonEmpty` rejects blank passwords.
- `hasMinLength 8` enforces a minimum password length.
- `isEqual passwordConfirmation` checks that the password and confirmation match.
- `validateIsUnique #email` queries the database to make sure the email is not already taken. Because it does IO, everything after it uses `>>=`.

### Registration View

Create `Web/View/Users/New.hs`:

```haskell
module Web.View.Users.New where
import Web.View.Prelude

data NewView = NewView { user :: User }

instance View NewView where
    html NewView { .. } = [hsx|
        <div class="mx-auto" style="max-width: 400px">
            <h1>Sign Up</h1>
            {renderForm user}
        </div>
    |]

renderForm :: User -> Html
renderForm user = formFor user [hsx|
    {(textField #email) { fieldLabel = "Email" }}
    {(passwordField #passwordHash) { fieldLabel = "Password", required = True }}
    {(passwordField #passwordHash) { fieldLabel = "Confirm Password", fieldName = "passwordConfirmation", required = True, validatorResult = Nothing }}
    {submitButton { label = "Sign Up" }}
|]
```

You can link to the registration page from your login view:

```html
<a href={NewUserAction}>Sign Up</a>
```

## Password Reset

IHP does not include a built-in password reset flow, but the pattern is straightforward to implement yourself. The general approach is:

1. User requests a password reset by entering their email.
2. Your app generates a random token, saves it to the user record, and emails a reset link.
3. User clicks the link. Your app verifies the token and shows a "set new password" form.
4. User submits a new password. Your app hashes and saves it.

### Database Columns

Add two columns to your `users` table:

```sql
ALTER TABLE users ADD COLUMN password_reset_token TEXT DEFAULT NULL;
ALTER TABLE users ADD COLUMN password_reset_token_created_at TIMESTAMP WITH TIME ZONE DEFAULT NULL;
```

### Controller Actions

Add these actions to `Web/Types.hs`:

```haskell
data PasswordResetsController
    = NewPasswordResetAction
    | CreatePasswordResetAction
    | EditPasswordResetAction { userId :: !(Id User), token :: !Text }
    | UpdatePasswordAction { userId :: !(Id User), token :: !Text }
    deriving (Eq, Show, Data)
```

Add routing in `Web/Routes.hs`:

```haskell
instance AutoRoute PasswordResetsController
```

Mount in `Web/FrontController.hs`:

```haskell
import Web.Controller.PasswordResets
-- ...
, parseRoute @PasswordResetsController
```

### Controller Implementation

Create `Web/Controller/PasswordResets.hs`:

```haskell
module Web.Controller.PasswordResets where

import Web.Controller.Prelude
import Web.View.PasswordResets.New
import Web.View.PasswordResets.Edit
import Web.Mail.PasswordResetMail
import IHP.AuthSupport.Authentication (generateAuthenticationToken, hashPassword)

instance Controller PasswordResetsController where
    -- Show "forgot password" form
    action NewPasswordResetAction = do
        render NewView

    -- Handle the form submission: generate token and send email
    action CreatePasswordResetAction = do
        let email = param @Text "email"
        maybeUser <- query @User
            |> filterWhereCaseInsensitive (#email, email)
            |> fetchOneOrNothing
        case maybeUser of
            Just user -> do
                token <- generateAuthenticationToken
                now <- getCurrentTime
                user
                    |> set #passwordResetToken (Just token)
                    |> set #passwordResetTokenCreatedAt (Just now)
                    |> updateRecord
                sendMail PasswordResetMail { .. }
            Nothing -> pure () -- Don't reveal whether the email exists
        setSuccessMessage "If that email is in our system, you will receive a reset link shortly."
        redirectTo NewPasswordResetAction

    -- Show "enter new password" form (after clicking email link)
    action EditPasswordResetAction { userId, token } = do
        user <- fetch userId
        -- Verify the token matches and is not older than 1 hour
        case user.passwordResetToken of
            Just storedToken | storedToken == token -> do
                case user.passwordResetTokenCreatedAt of
                    Just createdAt -> do
                        now <- getCurrentTime
                        let ageInSeconds = diffUTCTime now createdAt
                        if ageInSeconds < 3600
                            then render EditView { .. }
                            else do
                                setErrorMessage "This reset link has expired. Please request a new one."
                                redirectTo NewPasswordResetAction
                    Nothing -> redirectTo NewPasswordResetAction
            _ -> do
                setErrorMessage "Invalid reset link."
                redirectTo NewPasswordResetAction

    -- Save the new password
    action UpdatePasswordAction { userId, token } = do
        user <- fetch userId
        -- Re-verify the token (don't trust that EditPasswordResetAction already checked it)
        case user.passwordResetToken of
            Just storedToken | storedToken == token -> do
                let passwordConfirmation = param @Text "passwordConfirmation"
                let password = param @Text "passwordHash"
                if password /= passwordConfirmation
                    then do
                        setErrorMessage "Passwords don't match"
                        redirectTo (EditPasswordResetAction userId token)
                    else do
                        hashed <- hashPassword password
                        user
                            |> set #passwordHash hashed
                            |> set #passwordResetToken Nothing
                            |> set #passwordResetTokenCreatedAt Nothing
                            |> updateRecord
                        setSuccessMessage "Your password has been reset. Please log in."
                        redirectTo NewSessionAction
            _ -> do
                setErrorMessage "Invalid reset link."
                redirectTo NewPasswordResetAction
```

### Password Reset Views

Create `Web/View/PasswordResets/New.hs`:

```haskell
module Web.View.PasswordResets.New where
import Web.View.Prelude

data NewView = NewView

instance View NewView where
    html NewView = [hsx|
        <div class="mx-auto" style="max-width: 400px">
            <h1>Forgot Password</h1>
            <form method="POST" action={CreatePasswordResetAction}>
                <div class="mb-3">
                    <label class="form-label" for="email">Email</label>
                    <input name="email" id="email" type="email" class="form-control" required autofocus />
                </div>
                <button type="submit" class="btn btn-primary w-100">Send Reset Link</button>
            </form>
        </div>
    |]
```

Create `Web/View/PasswordResets/Edit.hs`:

```haskell
module Web.View.PasswordResets.Edit where
import Web.View.Prelude

data EditView = EditView { user :: User, token :: Text }

instance View EditView where
    html EditView { .. } = [hsx|
        <div class="mx-auto" style="max-width: 400px">
            <h1>Set New Password</h1>
            <form method="POST" action={UpdatePasswordAction user.id token}>
                <div class="mb-3">
                    <label class="form-label" for="passwordHash">New Password</label>
                    <input name="passwordHash" id="passwordHash" type="password" class="form-control" required />
                </div>
                <div class="mb-3">
                    <label class="form-label" for="passwordConfirmation">Confirm Password</label>
                    <input name="passwordConfirmation" id="passwordConfirmation" type="password" class="form-control" required />
                </div>
                <button type="submit" class="btn btn-primary w-100">Reset Password</button>
            </form>
        </div>
    |]
```

### Password Reset Email

Create `Web/Mail/PasswordResetMail.hs`:

```haskell
module Web.Mail.PasswordResetMail where
import Web.View.Prelude
import IHP.MailPrelude

data PasswordResetMail = PasswordResetMail { user :: User, token :: Text }

instance BuildMail PasswordResetMail where
    subject = "Reset Your Password"
    to PasswordResetMail { .. } = Address { addressName = Nothing, addressEmail = user.email }
    from = "noreply@example.com" -- Change to your app's email
    html PasswordResetMail { .. } = [hsx|
        <h1>Password Reset</h1>
        <p>Someone requested a password reset for your account. If this was you, click the link below:</p>
        <a href={urlTo (EditPasswordResetAction user.id token)} target="_blank">
            Reset Your Password
        </a>
        <p>This link will expire in 1 hour.</p>
        <p>If you did not request this, you can safely ignore this email.</p>
    |]
```

You can link to the password reset page from your login view:

```html
<a href={NewPasswordResetAction}>Forgot your password?</a>
```

## Session Configuration

IHP sessions are cookie-based. Session data is cryptographically signed and encrypted using a key stored at `Config/client_session_key.aes`. This key is generated automatically the first time your app starts.

### Default Settings

The default session cookie has these properties:

- **Max age:** 30 days
- **Path:** `/`
- **SameSite:** Lax (protects against CSRF)
- **HttpOnly:** True (JavaScript cannot read the cookie)
- **Secure:** True when your `baseUrl` starts with `https://`

### Customizing Session Settings

You can override the session cookie in `Config/Config.hs`:

```haskell
import qualified Web.Cookie as Cookie
import IHP.FrameworkConfig (defaultIHPSessionCookie)

config :: ConfigBuilder
config = do
    option $ SessionCookie (defaultIHPSessionCookie "https://yourapp.com")
        { Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 7)) -- 7 days instead of 30
        }
```

### How Login and Logout Work

When a user logs in, IHP stores the user's ID in the session under the key `login.User` (or `login.Admin` for admin authentication). The key is constructed from the model name, not the table name. The `authMiddleware @User` middleware (configured in `Config.hs`) reads this session value on each request and fetches the corresponding user record from the database.

When a user logs out, IHP sets the session value for `login.User` to an empty string. The session cookie itself remains, but the user ID is cleared.

### Working with the Session Directly

You can store and retrieve your own values in the session:

```haskell
-- Storing a value
action MyAction = do
    setSession "preferredLanguage" ("en" :: Text)

-- Retrieving a value
action AnotherAction = do
    language <- getSession @Text "preferredLanguage"
    -- language :: Maybe Text

-- Deleting a value
action LogoutAction = do
    deleteSession "preferredLanguage"
```

## Verifying Your Setup

After completing the authentication setup, walk through these checks to make sure everything is working.

### 1. Visit a Protected Page Without Logging In

Add `ensureIsUser` to a controller's `beforeAction`:

```haskell
instance Controller SomeController where
    beforeAction = ensureIsUser
```

Now visit that page in your browser. You should be redirected to `/NewSession` (the login page).

### 2. Try Logging In with Wrong Credentials

Go to `/NewSession` and enter an incorrect email or password. You should see the error message "Invalid Credentials" and remain on the login page.

### 3. Try Logging In with Correct Credentials

First, create a user. You can do this from the IHP Schema Designer by inserting a row into the `users` table. To generate a password hash, run this from your terminal:

```bash
hash-password
```

Enter a password and paste the resulting hash into the `password_hash` column.

Now go to `/NewSession` and log in with the correct email and password. You should be redirected to `/` (or whatever you configured as `afterLoginRedirectPath`).

### 4. Verify currentUser Works

In any action protected by `ensureIsUser`, add:

```haskell
action MyAction = do
    let email = currentUser.email
    renderPlain ("Logged in as: " <> email)
```

You should see your email displayed.

### 5. Test Logout

Click your logout link (pointing to `DeleteSessionAction`). You should be redirected back to the login page. Visiting a protected page should redirect to login again.

## Setup Checklist

Here is a summary of every change needed to add authentication. Use this as a reference to make sure you have not missed a step.

1. Add `id`, `email`, `password_hash`, `locked_at`, and `failed_login_attempts` columns to your `users` table in `Schema.sql`.

2. In `Web/Types.hs`:
    - Add `import IHP.LoginSupport.Types`
    - Add `instance HasNewSessionUrl User` with the login URL
    - Add `type instance CurrentUserRecord = User`
    - Add the `data SessionsController` type with `NewSessionAction`, `CreateSessionAction`, `DeleteSessionAction`

3. In `Web/Routes.hs`:
    - Add `instance AutoRoute SessionsController`

4. Create `Web/Controller/Sessions.hs`:
    - Delegate actions to `IHP.AuthSupport.Controller.Sessions`
    - Add `instance Sessions.SessionsControllerConfig User`

5. Create `Web/View/Sessions/New.hs`:
    - Implement the login form view

6. In `Web/FrontController.hs`:
    - Add `import Web.Controller.Sessions`
    - Mount the controller: `parseRoute @SessionsController`

7. In `Config/Config.hs`:
    - Add `import IHP.LoginSupport.Middleware`
    - Add `option $ AuthMiddleware (authMiddleware @User)`

8. Add `ensureIsUser` to `beforeAction` in any controller that requires login.

9. Add a logout link in your layout: `<a class="js-delete js-delete-no-confirm" href={DeleteSessionAction}>Logout</a>`

10. (Optional) Create a registration controller and view for user sign-up.

11. (Optional) Set up password reset flow with token generation and email.

[Next: Authorization](https://ihp.digitallyinduced.com/Guide/authorization.html)
