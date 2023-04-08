# Deployment

```toc

```

## Deploying with Shipnix

Shipnix is a service for deploying NixOS web servers on DigitalOcean with first-class support for IHP.

Generating a base NixOS configuration for your specific project, Shipnix provides full freedom to configure with ease of use.

### Account Setup

Register a new account on [shipnix.io](https://shipnix.io) and follow the instructions to connect to your DigitalOcean account and upload a NixOS image to it. This one-time process takes about 10 minutes.

### Creating a new project

Provisioning a new IHP project is straightforward with the IHP starter. Read [the IHP starter guide](https://docs.shipnix.io/starters/ihp/) to find out how to set domains, enable https with LetsEncrypt and other common usecases.

## Deploying with Docker

Deploying IHP with docker is a good choice for a professional production setup.

IHP has a first party CLI tool called `ihp-app-to-docker-image` to create Docker images out of your app. This tool is available [with IHP Pro and IHP Business](ihp-pro.html). If you're not on IHP Pro yet, now is a good time to try it out. By switching to Pro, you're supporting the sustainable development of IHP.

### Creating a Docker Image

Assuming your project is using IHP Pro or IHP Business, you can use the `ihp-app-to-docker-image` tool to make a docker image:

```bash
$ ihp-app-to-docker-image

...
✅ The docker image is at 'docker.tar.gz'
```

The command needs to be called from inside the application directory.

This tool will compile your app and output an docker image at `docker.tar.gz`. The docker image is typically around 85MB in size. If your application has many dependencies declated in the `default.nix` it could also be larger.

On macOS the `ihp-app-to-docker-image` tool requires Docker to be up and running. On linux you use the tool without having docker installed.

You can load the `docker.tar.gz` into your running docker instance using `docker load`:

```bash
$ docker load < docker.tar.gz

8e8f0ea2cd55: Loading layer [==================================================>]  87.73MB/87.73MB
Loaded image: app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

Running `docker images` you can now see that the image is available:

```bash
$ docker images

REPOSITORY     TAG                                IMAGE ID       CREATED         SIZE
app            g13rks9fb4ik8hnqip2s3ngqq4nq14zw   ffc01de1ec7e   51 years ago    86.6MB
```

The `CREATED` timestamp is showing `51 years ago` as the image is built using nix. For having a totally reproducable build, the timestamp is set to `Jan 1970, 00:00 UTC`.

### Starting the App Container

#### First Steps

You can start your app container like this:

```bash
$ docker run -p 8000:8000 app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

Now open `http://localhost:8000/` and see that your app is running. It will likely show an error that IHP is unable to connect to the app DB. This will be fixed in the next section.

The app image has to be the fully qualified `app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw` form, so this will not work:

```bash
$ docker run -p 8000:8000 app

Unable to find image 'app:latest' locally
```

#### Connecting the DB

You need to connect a postgres database to get your app working.

It's recommended to use a managed database service like AWS RDS to run your postgres. For a quick-and-dirty setup you can also use docker to run a database:

```bash
$ docker run --name app-db -e POSTGRES_PASSWORD=mysecretpassword -d postgres

# Import the Schema.sql
$ docker exec -i app-db psql -U postgres -d postgres < Application/Schema.sql
CREATE EXTENSION
CREATE TABLE
...

# Import the Fixtures.sql
$ docker exec -i app-db psql -U postgres -d postgres < Application/Fixtures.sql
ALTER TABLE
INSERT 0 1
INSERT 0 1
INSERT 0 1
...

```

You can configure the database your app connects to using the `DATABASE_URL` env variable:

```bash
$ docker run \
    -p 8000:8000 \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

#### Recommended Env Variables

##### `IHP_SESSION_SECRET`

In production setup's you want to configure the `IHP_SESSION_SECRET` env variable. It's a private key used to encrypt your session state. If it's not specified, a new one will generated on each container start. This means that all your users will have to re-login on each container start.

**Note on `Config/client_session_key.aes`:** The `IHP_SESSION_SECRET` env variable is an alternative for placing a `Config/client_session_key.aes` inside the container. It has been added in recent IHP versions only.

When you start an app container without specifying the `IHP_SESSION_SECRET`, the app will output the randomly generated one. So you can get a new secret key by starting a new container and copying the value:

```bash
$ docker run -it app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp
Server started
```

There we can copy the `IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp` value and use it as our secret:

```bash
$ docker run \
    -p 8000:8000 \
    -e 'IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp' \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

##### `IHP_ASSET_VERSION`

**As of IHP v0.16 this env variable is automatically set to a unqiue build hash.**

If you use [`assetPath` helpers](assets.html) in your app, specifiy the `IHP_ASSET_VERSION` env var. Set it e.g. to your commit hash or to the release timestamp.

```bash
$ docker run \
    -p 8000:8000 \
    -e 'IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp' \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    -e 'IHP_ASSET_VERSION=af5f389ef7a64a04c9fa275111e4739c0d4a78d0' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

##### `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE`

If the app is running behind a load balancer, set the environment variable `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader` to tell IHP to use the `X-Real-IP` or `X-Forwarded-For` header for detecting the client IP.

```bash
$ docker run \
    -e 'IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

##### `IHP_BASEURL`

Without specifying this env var, the app will always use `http://localhost:8000/` in absolute URLs it's generating (e.g. when redirecting or sending out emails).

It's therefore important to set it to the external user-facing web addresss. E.g. if your IHP app is available at `https://example.com/`, the variable should be set to that:

```bash
$ docker run \
    -e 'IHP_BASEURL=https://example.com' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

## Deploying on Bare Metal

You can build and deploy your IHP app on your own server without external deployment tools.

Make sure that the infrastructure you pick to build your IHP app has enough memory. Otherwise, the build might fail because GHC is very memory hungry. You can also set up a swap file to work around this.

### Install Nix on your server

Nix is needed to build your application. Install it the usual way:

```bash
curl -L https://nixos.org/nix/install | sh
```

We recommend to use the digitally induced cachix binary cache to avoid rebuilding the IHP dependencies and IHP itself:

```
cachix use digitallyinduced
```

In case you're on NixOS, you can skip this.

### Copy your project folder to your server

Copy your application source code to the build server. If you're using `git` to clone it onto your server, we recommend you use [`SSH agent forwarding`](https://docs.github.com/en/developers/overview/using-ssh-agent-forwarding).

### Configuration

IHP apps are typically configured using environment variables:

1. Set the env `IHP_ENV=Production` to enable production mode
2. Set `IHP_BASEURL=https://{yourdomain}`
3. Configure any custom settings
   (This includes ´make -B .envrc´ to download and build any extra Haskell packages, such as the mmark package in the tutorial)

If you deploy behind an Nginx proxy or similar which handles SSL certificates, so the IHP instance only sees http, the `IHP_BASEURL` must still have `https` as it is used to form absolute URLs.

To configure your database connection: Set the env var `DATABASE_URL` to your Postgres connection URL.
Set the env var `PORT` to the port the app will listen on.

The database needs the UUID-extension which is enabled by running `create extension if not exists "uuid-ossp";`

If the app is running behind a load balancer, set the environment variable `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader` to tell IHP to use the `X-Real-IP` or `X-Forwarded-For` header for detecting the client IP.

#### Tweaking memory usage and performance

IHP by default sets some default values for the GHC/Haskell Runtime System (RTS) which work well in production with high loads, at the cost of using a bit of memory. If you want your IHP deployment to use less RAM on your machine, try different values for the environment variable `GHCRTS`. The default value (set in IHP's [Makefile.dist](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/Makefile.dist#L86)) is `"-A128M -N3 -qn3 -G4"`. If you want very low memory usage, at the cost of more frequent garbage collection, try `export GHCRTS="-A16M -N"`. A middle ground is IHPCloud's default of `export GHCRTS="-A128M -N3 -qn3 -G4"`.

For explanations of these values, see GHC's [manual on the RTS](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html) and on [RTS options for concurrency]( https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism). In short, `-A` is the allocation area, `-G` is number of generations, `-qn` is number of threads to use for parallel GC, `-N` is number of threads and `-n` is the memory chunk area.

### Building

First run `make prepare-optimized-nix-build` to enable optimized binary builds. You can skip this step in case you want faster build times, and are fine with slower runtime performance.

Inside your project directory call `nix-build`. This will trigger a full clean build and place the output at `./result`.

After the build has finished, you can find the production binary at `result/bin/RunProdServer`.

### Starting the app

Now you should be able to start your app by running `result/bin/RunProdServer`.

## CSS & JS Bundling

Bundling all your CSS and JS files into a single CSS and JS file can be useful to improve performance when you have many assets.

### Caching

To decide whether bundling is useful for your application it might be useful to quickly go into details about the browser caching that is applied to all static files.

In production mode IHP automatically adds caching headers to your CSS and JS files following these rules:

1. `static/vendor/*`: cached for 30 days
2. `static/*` cached for 24 hours
3. IHP built-ins (e.g. `helpers.js`, `ihp-auto-refresh.js`, ..): cached for 30 days

This means that all JS and CSS is stored in the browser cache after the first request. This also means that bundling only improves the cache-miss case. To optimise the first request it could be useful to move more scripts from the `<head>` to the end of the `<body>` in your `Layout.hs`. Usually not all CSS and JS is needed when the page is first displayed.

If you have many JS and CSS files that are all required for the initial page render, you should enable bundling.

If you're curious: The following cache headers are set in production:

-   `Cache-Control`
-   `Last-Mofified`
-   `ETag`

### Activate Bundling

IHP provides a simple bundling out of the box using `make`. These bundles can be generated using these commands:

```bash
make static/prod.js # Generates a bundle at static/prod.js
make static/prod.css # Generates a bundle at static/prod.css
```

The bundling process is only concatenating the files (along the lines of `cat a.css b.css c.css > static/prod.css`). There is no minification or transpiling applied.

Bundling should only be used in production. In development mode you should generally not use the bundling mechanism.

#### Configuring the CSS & JS Bundling

The files that are bundled together to get our `prod.css` and `prod.js` are configured in your projects `Makefile`. Inside your projects `Makefile` you will have these statements:

```bash
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css

JS_FILES += ${IHP}/static/vendor/jquery-3.6.0.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper-2.11.6.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
JS_FILES += ${IHP}/static/vendor/flatpickr.js
JS_FILES += ${IHP}/static/helpers.js
JS_FILES += ${IHP}/static/vendor/morphdom-umd.min.js
JS_FILES += ${IHP}/static/vendor/turbolinks.js
JS_FILES += ${IHP}/static/vendor/turbolinksInstantClick.js
JS_FILES += ${IHP}/static/vendor/turbolinksMorphdom.js
```

You need to add your app-specific CSS and JS files here as well. E.g. if you have an `app.css`, `layout.css` and `app.js` add them by appending this:

```bash
CSS_FILES += static/app.css
CSS_FILES += static/layout.css
JS_FILES += static/app.js
```

Run `make static/prod.js static/prod.css` to test that the bundle generation works locally. To force a rebuild, either delete the files and run make again, or run `make -B static/prod.js static/prod.css`.

You can also remove the JS and CSS files that are provided by IHP (like `${IHP}/static/vendor/bootstrap.min.css`) if you don't need them. E.g. if you don't use bootstrap for your CSS, just remove the `CSS_FILES` and `JS_FILES` statements for bootstrap.

**Note on CSS Imports:**

If your app.css uses `@import` syntax like this:

```css
@import './layout.css';
@import './startpage.css';
```

Browsers only load these `@import` statements if they're the first rules defined in your CSS file. When bundling your file, you usually have your CSS frameworks and libraries first before your own app specific CSS. That means that the `@import` statements will be ignored by the browser in production. To make this work in production you need to duplicate these import statements in your `Makefile` like this:

```makefile
# CSS Frameworks
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css

CSS_FILES += static/app.css # The main app.css
CSS_FILES += static/layout.css # The first import of app.css
CSS_FILES += static/startpage.css # The second import of app.css
```

### Enabling Bundling in the Layout

We need to update the `Layout.hs` to only load `prod.css` and `prod.js` when running in production.

For that we use [`isDevelopment`](https://ihp.digitallyinduced.com/api-docs/IHP-FrameworkConfig.html#v:isDevelopment) and [`isProduction`](https://ihp.digitallyinduced.com/api-docs/IHP-FrameworkConfig.html#v:isProduction) to conditionally load different files. Change your `Web/View/Layout.hs` to look like this:

```haskell
stylesheets :: Html
stylesheets = do
    when isDevelopment [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]
    when isProduction [hsx|
        <link rel="stylesheet" href="/prod.css"/>
    |]

scripts :: Html
scripts = do
    when isDevelopment [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.6.0.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper-2.11.6.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
    |]
    when isProduction [hsx|
        <script src="/prod.js"></script>
    |]
```

### Updating your Deployment Process

**If you're using Nix:** Nothing to do. The command `make static/prod.js static/prod.css` is automatically executed during deployment.

**If you're deploying manually:** Make sure that `make static/prod.js static/prod.css` is called.

## Operating an IHP app

### Error Monitoring with Sentry

In production it's highly recommended to use an exception tracking service like [Sentry](https://sentry.io/) for error monitoring.

To use sentry in your IHP app you need to install the ihp-sentry plugin. The ihp-sentry plugin is bundled with IHP Pro.

Once the `ihp-sentry` plugin is installed and configured, exceptions that happen in production (so `option Production` is set) are reported to sentry.

#### Install ihp-sentry in your IHP app

Add `ihp-sentry` to the `haskellDeps` in your `default.nix`:

```nix
let
    ...
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            # ...
            ihp-sentry
        ];
    ...
```

Now you need to remake your environment using `make -B .envrc`.

Next add `import IHP.Sentry` to your `Config/Config.hs`:

```haskell
module Config where

-- ...

import IHP.Sentry
```

Add a call to `initSentry` inside the `Config/Config.hs` to configure the sentry DSN:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Sentry

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initSentry "YOUR-SENTRY-DSN"
```

Now sentry is set up.

## Building with Nix

You can use `nix-build` to make a full build of your IHP app:

```
# Optional, if you skip this the binary will not be optimized by GHC
make prepare-optimized-nix-build

# The actual build process
nix-build
```

This will build a nix package that contains the following binaries:

-   `RunProdServer`, the binary to start web server
-   `RunJobs`, if you're using the IHP job queue, this binary will be the entrypoint for the workers
-   a binary for each script in `Application/Script`, e.g. `Welcome` for `Application/Script/Welcome.hs`

The build contains an automatic hash for the `IHP_ASSET_VERSION` env variable, so cache busting should work out of the box.


# Env Var Reference

Your IHP app can be configured at runtime with environment variables.

## Recommended Env Vars


#### `IHP_ENV`

Switch IHP to production mode like this:

```bash
$ export IHP_ENV="Production"
$ ./build/bin/RunProdServer
```

The production mode has effects on caching behaviour of static files, logging and error rendering.

You can also use `IHP_ENV=Development` to force dev mode.


#### `IHP_BASEURL`

Without specifying this env var, the app will always use `http://localhost:8000/` in absolute URLs it's generating (e.g. when redirecting or sending out emails).

It's therefore important to set it to the external user-facing web addresss. E.g. if your IHP app is available at `https://example.com/`, the variable should be set to that:

```bash
$ export IHP_BASEURL=https://example.com
$ ./build/bin/RunProdServer
```

#### `IHP_SESSION_SECRET`

In production setup's you want to configure the `IHP_SESSION_SECRET` env variable. It's a private key used to encrypt your session state. If it's not specified, a new one will generated on each container start. This means that all your users will have to re-login on each container start.

**Note on `Config/client_session_key.aes`:** The `IHP_SESSION_SECRET` env variable is an alternative for placing a `Config/client_session_key.aes` inside the your repository. If IHP detects a `Config/` folder, and no `IHP_SESSION_SECRET` is set, it will automatically create a `Config/client_session_key.aes` file. This is designed for persistent sessions in development mode.

When you start an app without specifying the `IHP_SESSION_SECRET` and no `Config/client_session_key.aes` is found, the app will output the randomly generated one. So you can get a new secret key by starting a new container and copying the value:

```bash
$ ./build/bin/RunProdServer
IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp
Server started
```

There we can copy the `IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp` value and use it as our secret:

```bash
$ export IHP_SESSION_SECRET="1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp"
$ ./build/bin/RunProdServer
```

#### `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE`

If the app is running behind a load balancer, set the environment variable `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader` to tell IHP to use the `X-Real-IP` or `X-Forwarded-For` header for detecting the client IP.

```bash
$ export IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader
$ ./build/bin/RunProdServer
```

#### `PORT`

Specifies the TCP Port where the application will listen to. Defaults to `8000`.

```bash
$ export PORT=1337
$ ./build/bin/RunProdServer
# App now starts on port 1337 instead of 8000
```

#### `DATABASE_URL`

You can configure the database your app connects to using the `DATABASE_URL` env variable:

```bash
$ export DATABASE_URL="postgresql://postgres:mysecretpassword@the-hostname/postgres"
$ ./build/bin/RunProdServer
```


## Advanced Env Vars

#### `IHP_ASSET_VERSION`

As of IHP v0.16 this env variable is automatically set to a unique build hash.

If you use [`assetPath` helpers](assets.html) in your app, specifiy the `IHP_ASSET_VERSION` env var. Set it e.g. to your commit hash or to the release timestamp.

```bash
$ export IHP_ASSET_VERSION=af5f389ef7a64a04c9fa275111e4739c0d4a78d0
$ ./build/bin/RunProdServer
```

#### `GHCRTS`

IHP by default sets some default values for the GHC/Haskell Runtime System (RTS) which work well in production with high loads, at the cost of using a bit of memory. If you want your IHP deployment to use less RAM on your machine, try different values for the environment variable `GHCRTS`. The default value (set in IHP's [Makefile.dist](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/Makefile.dist#L86)) is `"-A128M -N3 -qn3 -G4"`. If you want very low memory usage, at the cost of more frequent garbage collection, try `export GHCRTS="-A16M -N"`. A middle ground is IHPCloud's default of `export GHCRTS="-A128M -N3 -qn3 -G4"`.

For explanations of these values, see GHC's [manual on the RTS](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html) and on [RTS options for concurrency]( https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism). In short, `-A` is the allocation area, `-G` is number of generations, `-qn` is number of threads to use for parallel GC, `-N` is number of threads and `-n` is the memory chunk area.

#### `IHP_IDE_BASEURL`

Only relevant in dev mode. This defaults to `IHP_IDE_BASEURL=http://localhost:8001`.

#### `IHP_RLS_AUTHENTICATED_ROLE`

A database role used by IHP DataSync. Defaults to `ihp_authenticated`.

#### `IHP_DATASYNC_MAX_SUBSCRIPTIONS_PER_CONNECTION`

The maximum number of subscriptions per websocket connection in IHP DataSync. Defaults to `128`.

#### `IHP_DATASYNC_MAX_TRANSACTIONS_PER_CONNECTION`

The maximum number of database transactions per websocket connection in IHP DataSync. Defaults to `10`.
