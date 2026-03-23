# Security

```toc

```

## Overview

IHP provides several security protections out of the box. Many common web vulnerabilities -- such as cross-site scripting (XSS), SQL injection, and cross-site request forgery (CSRF) -- are handled automatically by the framework.

This guide explains what IHP protects against automatically and what you need to handle yourself.

## Cross-Site Scripting (XSS) Protection

Cross-site scripting (XSS) is a vulnerability where an attacker injects malicious scripts into web pages viewed by other users. If user input is rendered as raw HTML, an attacker could inject JavaScript that steals session cookies, redirects users, or modifies page content.

### Automatic Protection via HSX

IHP's HSX templating system automatically escapes all interpolated values. When you write:

```haskell
[hsx|<div>{userInput}</div>|]
```

Any HTML special characters in `userInput` are escaped before rendering. For example, if `userInput` contains `<script>alert('xss')</script>`, HSX will render it as the literal text `&lt;script&gt;alert('xss')&lt;/script&gt;` -- the browser will display it as text, not execute it as code.

This applies to all data types interpolated into HSX, including `Text`, `String`, `Int`, and any type with a `Show` instance. Attribute values are also escaped:

```haskell
[hsx|<a href={userProvidedUrl}>Link</a>|]
```

### When You Bypass Escaping

If you need to render raw HTML (for example, HTML generated from Markdown), you can use `preEscapedToHtml`:

```haskell
[hsx|<div>{markdownHtml |> preEscapedToHtml}</div>|]
```

**This bypasses XSS protection.** Only use `preEscapedToHtml` with content you have generated yourself or sanitized. Never use it with user-provided input:

```haskell
-- DANGEROUS: User input rendered as raw HTML
[hsx|<div>{param "comment" |> preEscapedToHtml}</div>|]

-- SAFE: Let HSX escape the input automatically
[hsx|<div>{param "comment"}</div>|]
```

Similarly, `preEscapedTextValue` bypasses escaping for attribute values. Use it with caution.

## SQL Injection Protection

SQL injection is a vulnerability where an attacker manipulates database queries by inserting malicious SQL through user input. For example, a user could type `'; DROP TABLE users; --` into a form field, and if the input is concatenated directly into a SQL query, the database would execute the injected command.

### Automatic Protection via QueryBuilder

IHP's QueryBuilder always uses parameterized queries. User input is sent as separate parameters to PostgreSQL, never interpolated into the SQL string:

```haskell
-- Safe: email is sent as a parameter, not concatenated into SQL
query @User
    |> filterWhere (#email, userProvidedEmail)
    |> fetchOneOrNothing
```

All QueryBuilder functions -- `filterWhere`, `filterWhereIn`, `filterWhereLike`, and others -- use parameterized queries. You do not need to do anything special to be protected against SQL injection when using the QueryBuilder.

### Raw SQL Queries

When you use `sqlQuery` or `sqlExec` for raw SQL, use `?` placeholders for parameters:

```haskell
-- SAFE: Using parameterized query
users <- sqlQuery "SELECT * FROM users WHERE email = ? AND active = ?" (email, True)
```

Never concatenate user input directly into a SQL string:

```haskell
-- DANGEROUS: SQL injection vulnerability
users <- sqlQuery ("SELECT * FROM users WHERE email = '" <> userInput <> "'") ()
```

The same applies to `typedSql`, which also uses parameterized queries:

```haskell
-- SAFE: Parameters are interpolated safely
users <- sqlQueryTyped [typedSql|
    SELECT id, email FROM users WHERE email = ${userEmail}
|]
```

## Cross-Site Request Forgery (CSRF)

Cross-site request forgery (CSRF) is an attack where a malicious website tricks a user's browser into making an unwanted request to your application. For example, if a user is logged in to your app, a malicious site could include a hidden form that submits a POST request to your app's "delete account" endpoint.

### How IHP Protects Against CSRF

IHP uses the `SameSite=Lax` cookie attribute on session cookies. This tells the browser to not send the session cookie along with cross-origin form submissions (POST requests). Since the session cookie is not sent, the attacker's request will not be authenticated, and the action will fail.

The `SameSite=Lax` policy allows normal top-level navigation (like clicking a link to your site from another site) but blocks cross-origin POST, PUT, and DELETE requests from sending cookies. This protects against all common CSRF attack vectors.

IHP does not use CSRF tokens. The `SameSite` cookie attribute provides equivalent protection and is supported by all modern browsers.

### What You Need to Do

Use POST (or DELETE/PUT/PATCH) requests for any action that modifies data. Do not use GET requests for side effects:

```haskell
-- Correct: Use a form with POST for state-changing actions
[hsx|
<form method="POST" action={DeletePostAction post.id}>
    <input type="hidden" name="_method" value="DELETE"/>
    <button type="submit">Delete</button>
</form>
|]

-- Wrong: Using a plain link for a destructive action
-- (GET requests are not protected by SameSite=Lax)
[hsx|<a href={DeletePostAction post.id}>Delete</a>|]
```

The `js-delete` CSS class is a special case -- IHP's JavaScript helpers will intercept the click and submit a proper DELETE request via a dynamically created form. But for all other side-effect actions, use a form with the appropriate method.

See the [Forms guide](https://ihp.digitallyinduced.com/Guide/form.html) for more details on form submissions and the `js-delete` helper.

## Session Security

Sessions store small amounts of data (like the current user ID) that persist between requests.

### How Sessions Work

IHP stores session data inside a **cryptographically signed and encrypted cookie** on the client. The encryption key is generated automatically and stored at `Config/client_session_key.aes`. Internally, IHP uses the [clientsession](https://hackage.haskell.org/package/clientsession-0.9.1.2/docs/Web-ClientSession.html) library.

Because the cookie is encrypted and signed, users cannot read or tamper with the session data.

### Cookie Security Flags

IHP sets the following security flags on session cookies by default:

| Flag | Value | Effect |
|------|-------|--------|
| `HttpOnly` | `True` | The cookie is not accessible from JavaScript, preventing XSS attacks from stealing session data |
| `SameSite` | `Lax` | The cookie is not sent with cross-origin POST requests, preventing CSRF attacks |
| `Secure` | Automatic | Set to `True` when your `baseUrl` starts with `https://`, ensuring the cookie is only sent over encrypted connections |
| `Max-Age` | 30 days | The session expires after 30 days of inactivity |
| `Path` | `/` | The cookie is available for all paths on the domain |

### Customizing Session Settings

You can customize the session cookie in `Config/Config.hs`:

```haskell
-- Change the session lifetime to 90 days
config :: ConfigBuilder
config = do
    option $ SessionCookie (defaultIHPSessionCookie "https://yourapp.com")
        { Cookie.setCookieMaxAge = Just (fromIntegral (60 * 60 * 24 * 90))
        }
```

### Managing the Session Secret

In production, the session encryption key can be provided via:

- `IHP_SESSION_SECRET` environment variable (the key value itself)
- `IHP_SESSION_SECRET_FILE` environment variable (path to a file containing the key)
- `Config/client_session_key.aes` file (used by default in development)

Keep the session secret safe. If it is compromised, an attacker can forge session cookies.

## Authentication Security

IHP provides a built-in authentication module. See the [Authentication guide](https://ihp.digitallyinduced.com/Guide/authentication.html) for setup instructions.

### Password Hashing

IHP uses the [pwstore-fast](https://hackage.haskell.org/package/pwstore-fast) library to hash passwords. Passwords are stored as salted hashes using the PBKDF1 algorithm with a strength factor of 17. Each password gets a unique random salt, so identical passwords produce different hashes.

Use `hashPassword` to hash passwords before storing them:

```haskell
action CreateUserAction = do
    newRecord @User
        |> fill @'["email", "passwordHash"]
        |> validateField #passwordHash nonEmpty
        |> ifValid \case
            Left user -> render NewView { .. }
            Right user -> do
                hashed <- hashPassword user.passwordHash
                user
                    |> set #passwordHash hashed
                    |> createRecord
                redirectTo UsersAction
```

### Account Lockout

IHP provides automatic account lockout to prevent brute-force attacks. After **10 failed login attempts** (by default), the user account is locked for one hour.

The lockout threshold is configurable in your `SessionsControllerConfig` instance:

```haskell
instance Sessions.SessionsControllerConfig User where
    maxFailedLoginAttempts _ = 5  -- Lock after 5 attempts instead of 10
```

This requires your `users` table to have `locked_at` and `failed_login_attempts` columns, which are part of the standard authentication schema.

### Timing-Safe Login

When a user provides an email that does not exist, IHP returns the same generic "Invalid Credentials" error message as when the password is wrong. This prevents attackers from enumerating valid email addresses.

## Mass Assignment Protection

Mass assignment is a vulnerability where an attacker submits extra form fields to modify database columns that were not intended to be user-editable. For example, a user registration form might only show name and email fields, but an attacker could add an `isAdmin=true` parameter to the request.

### How IHP Prevents Mass Assignment

IHP uses the `fill` function, which takes a type-level list of field names. Only the fields you explicitly list will be read from the request:

```haskell
action UpdateUserAction { userId } = do
    user <- fetch userId
    user
        |> fill @'["firstname", "lastname", "email"]
        |> ifValid \case
            Left user -> render EditView { .. }
            Right user -> do
                user |> updateRecord
                redirectTo ShowUserAction { userId }
```

In this example, only `firstname`, `lastname`, and `email` are read from the request parameters. Even if an attacker submits `isAdmin=true` or `passwordHash=...` in the request, those fields will not be set on the record.

This is similar to Rails' Strong Parameters, but enforced at the type level. If you try to `fill` a field that does not exist on the record, you get a compile-time error.

### Best Practice

Always use `fill` to whitelist the fields that should be settable by the user. Never manually read parameters and assign them to sensitive fields:

```haskell
-- SAFE: Only explicitly listed fields are filled from the request
user |> fill @'["firstname", "lastname"]

-- DANGEROUS: Manually reading and setting a sensitive field from user input
let isAdmin = param "isAdmin"
user |> set #isAdmin isAdmin
```

## File Upload Security

IHP supports file uploads to local storage and S3-compatible cloud storage. See the [File Storage guide](https://ihp.digitallyinduced.com/Guide/file-storage.html) for detailed usage.

### What IHP Does

- **Unique file paths**: Uploaded files are stored with UUID-based paths (e.g., `/uploads/users/550e8400-.../picture.jpg`), preventing file name collisions and path traversal attacks.
- **Cloud storage**: When using S3 storage, uploaded files are stored outside the web server's document root, and access is controlled via signed URLs with configurable expiration times.

### What You Need to Do

- **Validate file types**: IHP does not automatically validate file content types. If your application only accepts images, check the content type before storing:

    ```haskell
    action UploadAction = do
        case fileOrNothing "avatar" of
            Just fileInfo
                | fileInfo.fileContentType `elem` ["image/jpeg", "image/png"] ->
                    storeFile fileInfo "avatars"
                | otherwise -> do
                    setErrorMessage "Only JPEG and PNG files are allowed"
                    redirectTo UploadFormAction
            Nothing -> do
                setErrorMessage "No file selected"
                redirectTo UploadFormAction
    ```

- **Limit upload sizes**: Configure the maximum request body size in `Config/Config.hs` to prevent denial-of-service attacks from oversized uploads:

    ```haskell
    import qualified Network.Wai.Parse as WaiParse

    config :: ConfigBuilder
    config = do
        option $ WaiParse.setMaxRequestFileSize (10 * 1024 * 1024) -- 10 MB
            WaiParse.defaultParseRequestBodyOptions
    ```

- **Serve uploaded files safely**: When using local storage, uploaded files are stored in the `static/` directory and served directly by the web server. Consider using cloud storage (S3) in production so that file access can be controlled via signed, time-limited URLs.

## Environment Variables and Secrets

### Storing Secrets

Never hardcode secrets (database credentials, API keys, encryption keys) in your source code. Use environment variables:

```haskell
-- In your controller or config
import IHP.EnvVar

action SendEmailAction = do
    apiKey <- env @Text "SENDGRID_API_KEY"
    -- Use apiKey...
```

### Common Environment Variables

| Variable | Purpose |
|----------|---------|
| `DATABASE_URL` | PostgreSQL connection string |
| `IHP_SESSION_SECRET` | Session encryption key |
| `PORT` | HTTP server port |
| `IHP_BASEURL` | Base URL of the application (e.g., `https://yourapp.com`) |
| `IHP_ENV` | Environment (`Development` or `Production`) |

### Deployment

In production, set environment variables through your hosting provider's configuration panel, a `.env` file (not committed to version control), or a secrets manager. See the [Config guide](https://ihp.digitallyinduced.com/Guide/config.html) and [Deployment guide](https://ihp.digitallyinduced.com/Guide/deployment.html) for more details.

## HTTPS and Secure Headers

### HTTPS

IHP does not terminate TLS itself. In production, you should run IHP behind a reverse proxy (such as nginx or Caddy) or a load balancer that handles TLS termination.

When your `baseUrl` (set via `IHP_BASEURL` or in `Config/Config.hs`) starts with `https://`, IHP automatically sets the `Secure` flag on session cookies, ensuring they are only sent over encrypted connections.

Make sure your `IHP_BASEURL` uses `https://` in production:

```bash
export IHP_BASEURL="https://yourapp.com"
```

### Security Headers

IHP does not set security headers like `Strict-Transport-Security`, `X-Content-Type-Options`, or `X-Frame-Options` by default. You can add these using a custom middleware in `Config/Config.hs`:

```haskell
import Network.Wai (ifRequest, modifyResponse, mapResponseHeaders)

config :: ConfigBuilder
config = do
    option $ CustomMiddleware securityHeadersMiddleware

securityHeadersMiddleware :: Middleware
securityHeadersMiddleware application request respond =
    application request $ respond . addHeaders
    where
        addHeaders response = mapResponseHeaders (++ securityHeaders) response
        securityHeaders =
            [ ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
            , ("X-Content-Type-Options", "nosniff")
            , ("X-Frame-Options", "DENY")
            , ("Referrer-Policy", "strict-origin-when-cross-origin")
            ]
```

If your reverse proxy (nginx, Caddy, etc.) already adds these headers, you do not need to add them in IHP as well.

## Content Security Policy

A Content Security Policy (CSP) tells the browser which sources of content (scripts, styles, images, etc.) are allowed on your pages. This provides an additional layer of defense against XSS attacks.

IHP does not set a CSP header by default. You can add one using the same custom middleware approach shown above:

```haskell
securityHeaders =
    [ ("Content-Security-Policy", "default-src 'self'; script-src 'self'; style-src 'self' 'unsafe-inline'")
    -- ... other headers
    ]
```

Adjust the policy to match your application's needs. If you use inline scripts, external CDNs, or third-party services, you will need to add their domains to the appropriate directives. See the [MDN CSP documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP) for a full reference.

## Security Checklist

Use this checklist to review your application's security posture:

- [ ] **Use HTTPS in production** -- Set `IHP_BASEURL` to an `https://` URL so session cookies are marked `Secure`
- [ ] **Keep secrets out of source code** -- Store API keys, database credentials, and session secrets in environment variables, not in committed files
- [ ] **Use `fill` to whitelist form fields** -- Never manually assign user input to sensitive record fields
- [ ] **Do not use `preEscapedToHtml` with user input** -- Only use it with content you have generated or sanitized yourself
- [ ] **Use parameterized queries for raw SQL** -- Always use `?` placeholders in `sqlQuery`/`sqlExec`, never string concatenation
- [ ] **Validate file uploads** -- Check content types and set upload size limits
- [ ] **Use POST for state-changing actions** -- Do not use GET requests for actions that create, update, or delete data
- [ ] **Add security headers** -- Configure `Strict-Transport-Security`, `X-Content-Type-Options`, and `X-Frame-Options` via middleware or your reverse proxy
- [ ] **Keep dependencies updated** -- Run `nix flake update` periodically to pick up security patches
- [ ] **Use authorization checks** -- Use `ensureIsUser`, `accessDeniedUnless`, and row-level security policies to control access to resources
