# Deployment

```toc

```

## Deploying with IHP Cloud

The fastest way to share your app with the internet is by using IHP Cloud. We recommend following this approach as it's the most simple to get started with.

### Account Setup

IHP Cloud is currently still in beta. To register a new account you need an invite from an existing user or you can join the waiting list at [ihpcloud.com](https://ihpcloud.com/).

### Creating the Project

Once you're logged in to IHP Cloud, follow the instructions to create a project for your application.

**Private Git Repositories:**
When you connect a private git repository make sure to provide an SSH git clone URL. IHP Cloud will provide you with an SSH public key. You need to add this deploy key to your repository.

On GitHub, you can do this by opening the repository settings and clicking on `Deploy keys`.

### First Deployment

After the project setup is finished, click on `+ Deploy Now` to start your first deployment.

When your CSS or JS looks broken, take a look at the next section `CSS & JS Bundling`.

### DB Migrations

Currently, IHP has no standard way of doing migrations. Therefore currently you need to manually migrate your IHP Cloud database after deploying.

Open the project in IHP Cloud, click `Settings`, then click `Database`. There you can find the database credentials for the Postgres DB that is running for your application. Connect to your database and manually apply the migrations.

### Changing the domain

Open the project in IHP Cloud, click `Settings` and click `Domain`. You can set a `***.ihpapp.com` domain in here.

#### Using your domain instead of .ihpapp.com

Using your domain with IHP Cloud is only available for IHP Cloud Pro users.
To use your domain point a CNAME record to `ihpapp.com`.

After that go to `Settings`, click `Domain` and enter your domain name.
When you change your domain to a custom domain we are automatically getting an SSL certificate from
LetsEncrypt for you so please make sure to set the CNAME record a few minutes before changing the domain inside your project.

## Deploying manually

You can build and deploy your IHP app yourself.

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

Make required modifications to your `Config/Config.hs`:

1. Switch `option Development` to `option Production`
2. Set `option (AppHostname "YOUR_HOSTNAME")` or `option (BaseUrl "YOUR_URL")`
3. Configure any custom settings
   (This includes ´make -B .envrc´ to download and build any extra Haskell packages, such as the mmark package in the tutorial)

`AppHostname` is used to build your `BaseUrl` when this is not set manually.
`BaseUrl` equals `http://{AppHostname}:{port}` or `http://{AppHostname}` if port is 80.
You can overwrite `BaseUrl` by setting it in `Config/Config.hs`

If you deploy behind an Nginx proxy or similar which handles SSL certificates, so the IHP instance only sees http, the BaseUrl must still have `https` as it is used to form absolute URLs.

When you deploy with IHP Cloud your `Config.hs` is set automatically on project creation.
IHP Cloud sets your `BaseUrl` to `https://{AppHostname}` because every deployed app is served with SSL enabled.

To configure your database connection: Set the env var `DATABASE_URL` to your Postgres connection URL.
Set the env var `PORT` to the port the app will listen on.

The database needs the UUID-extension which is enabled by running `create extension if not exists "uuid-ossp";`

### Building

Inside your project directory start a `nix-shell` for the following steps.

We can use `make` to build the application binary. The default IHP Makefile provides two target: `build/bin/RunUnoptimizedProdServer` and `build/bin/RunOptimizedProdServer`.

The first target runs with `-O0`. It's useful when you're setting up the deployment workflow for the first time. Otherwise, you will always need to spend lots of time waiting for GHC to optimize your Haskell code.

Both make targets will generate a binary at `build/bin/RunUnoptimizedProdServer` or `build/bin/RunOptimizedProdServer` and will create a symlink at `build/bin/RunProdServer` targeting to the binary.

Run `make static/prod.css static/prod.js` to build the asset bundles.

### Starting the app

Now you should be able to start your app by running `build/bin/RunProdServer`.

### More Resources

[Check out this blog post on how to deploy IHP to EC2](http://harlambert.co.uk/ihp_notes/).

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
- `Cache-Control`
- `Last-Mofified`
- `ETag`

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

JS_FILES += ${IHP}/static/vendor/jquery-3.2.1.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper.min.js
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
@import "./layout.css";
@import "./startpage.css";
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

For that we use `isDevelopment` and `isProduction` to conditionally load different files. Change your `Web/View/Layout.hs` to look like this:

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
        <script src="/vendor/jquery-3.2.1.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper.min.js"></script>
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

**If you're using IHP Cloud:** Nothing to do. The command `make static/prod.js static/prod.css` is automatically executed during deployment.

**If you're deploying manually:** Make sure that `make static/prod.js static/prod.css` is called.
