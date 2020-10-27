## Description
This document describes breaking changes, as well as how to fix them, that have occured at given releases.
After updating your project, please consult the segments from your current release until now.

## In upcoming releases:

*Please add a section here when making a PR containing breaking changes.*

### FrameworkConfig is now a datatype

In order to remove a number of unsafeIO operations, the FrameworkConfig has been refactored to a datatype. 
Furthermore, the construction of the FrameworkConfig now leverages a state monad. 
To changes config values use the `option` function:

```haskell
-- Config/Config.hs

config :: ConfigBuilder
config = do
    option Development
    option $ AppHostname "localhost"

```

The option function uses a typemap to assign the values to different properties.
For generally used types, used as the `appHostName :: Text` a newtype has been introduced
The config now also has to be explicitly passed to the `Server.run` function.

```haskell
-- Main.hs

main :: IO ()
main = IHP.Server.run config
```

If you wish to access configuration properties in Controllers, Views or Scripts, then you can leverage the set of config functions:

```haskell
  configAppHostname :: (?requestContext :: RequestContext) => Text
  configAppHostname = (FrameworkConfig.appHostname . frameworkConfig) ?requestContext
   
  configEnvironment :: (?requestContext :: RequestContext) => Environment
  configEnvironment = (FrameworkConfig.environment . frameworkConfig) ?requestContext
   
  configAppPort :: (?requestContext :: RequestContext) => Int
  configAppPort = (FrameworkConfig.appPort . frameworkConfig) ?requestContext
   
  configBaseUrl :: (?requestContext :: RequestContext) => Text
  configBaseUrl = (FrameworkConfig.baseUrl . frameworkConfig) ?requestContext
   
  configRequestLoggerMiddleware :: (?requestContext :: RequestContext) => Middleware
  configRequestLoggerMiddleware = (FrameworkConfig.requestLoggerMiddleware . frameworkConfig) ?requestContext
   
  configSessionCookie :: (?requestContext :: RequestContext) => Cookie.SetCookie
  configSessionCookie = (FrameworkConfig.sessionCookie . frameworkConfig) ?requestContext
   
  configMailServer :: (?requestContext :: RequestContext) => MailServer
  configMailServer = (FrameworkConfig.mailServer . frameworkConfig) ?requestContext
   
  configDatabaseUrl :: (?requestContext :: RequestContext) => ByteString
  configDatabaseUrl = (FrameworkConfig.databaseUrl . frameworkConfig) ?requestContext
```

Which brings us the last change, namely that the functions described above have to be used in `Web/View/Layout.hs`

```haskell
-- Replace this syntax
when (isDevelopment FrameworkConfig.environment) 

-- With this
when (isDevelopment configEnvironment) 
```

Also define the type headers for all the functions in `Layout.hs` in order to capture the RequestContext:

```haskell
defaultLayout :: (?requestContext :: RequestContext) => Html -> Html
stylesheets :: (?requestContext :: RequestContext) => Html
scripts :: (?requestContext :: RequestContext) => Html 
metaTags :: (?requestContext :: RequestContext) => Html
```

