# Getting Started With Integrated Haskell Platform

IHP is a full-stack framework focused on rapid application development while striving for robust code quality.

We believe that functional programming is the future of software development and want to make functional programming with Haskell and Nix available to anyone. We try to offer a solution that can be used by developers who have not worked with Haskell yet. IHP comes with everything you need to build great web applications with Haskell and Nix. We have made a lot of pragmatic decisions to get you started faster. This way you can just pick up Haskell along the way :-)

## Learning Path

If you are new to IHP, follow these guides in order. Each stage builds on the previous one.

### 1. Getting Started

- [Installing IHP](https://ihp.digitallyinduced.com/Guide/installation.html) -- Set up your development environment with Nix
- [Your First Project](https://ihp.digitallyinduced.com/Guide/your-first-project.html) -- Build a working blog app step by step
- [Architecture](https://ihp.digitallyinduced.com/Guide/architecture.html) -- Understand how IHP projects are structured
- [Editors & Tooling](https://ihp.digitallyinduced.com/Guide/editors.html) -- Configure your editor for IHP development
- [Helpful Tips](https://ihp.digitallyinduced.com/Guide/helpful-tips.html) -- Practical advice for working with IHP and Haskell
- [Example Projects](https://ihp.digitallyinduced.com/Guide/examples.html) -- Sample apps to learn from and reference

### 2. Core Concepts

- [Database & Schema](https://ihp.digitallyinduced.com/Guide/database.html) -- Define your schema and work with PostgreSQL
- [Database Migrations](https://ihp.digitallyinduced.com/Guide/database-migrations.html) -- Evolve your schema over time with migrations
- [Routing](https://ihp.digitallyinduced.com/Guide/routing.html) -- Map URLs to controller actions with type-safe routes
- [Controller & Actions](https://ihp.digitallyinduced.com/Guide/controller.html) -- Handle requests, load data, and send responses
- [Views & HSX](https://ihp.digitallyinduced.com/Guide/view.html) -- Render HTML using IHP's view layer
- [HSX](https://ihp.digitallyinduced.com/Guide/hsx.html) -- Write HTML with JSX-like syntax inside Haskell
- [Forms](https://ihp.digitallyinduced.com/Guide/form.html) -- Build forms that map directly to your data models
- [Validation](https://ihp.digitallyinduced.com/Guide/validation.html) -- Validate user input with composable validators

### 3. Common Features

- [Authentication](https://ihp.digitallyinduced.com/Guide/authentication.html) -- Add user login and registration
- [Authorization](https://ihp.digitallyinduced.com/Guide/authorization.html) -- Control access to actions and resources
- [JSON API](https://ihp.digitallyinduced.com/Guide/json-api.html) -- Build JSON API endpoints for mobile apps, SPAs, and integrations
- [Session](https://ihp.digitallyinduced.com/Guide/session.html) -- Store and retrieve per-user session data
- [Mail](https://ihp.digitallyinduced.com/Guide/mail.html) -- Send emails from your application
- [File Storage & Uploads](https://ihp.digitallyinduced.com/Guide/file-storage.html) -- Handle file uploads with S3 or local storage
- [Jobs](https://ihp.digitallyinduced.com/Guide/jobs.html) -- Run background tasks with the built-in job queue
- [Scripts](https://ihp.digitallyinduced.com/Guide/scripts.html) -- Write standalone scripts that run in your app context
- [Flash Messages](https://ihp.digitallyinduced.com/Guide/flash-messages.html) -- Show success and error notifications to users
- [Modal](https://ihp.digitallyinduced.com/Guide/modal.html) -- Display modal dialogs in your views

### 4. Frontend

- [Auto Refresh](https://ihp.digitallyinduced.com/Guide/auto-refresh.html) -- Push live updates to the browser without JavaScript
- [htmx & Hyperscript](https://ihp.digitallyinduced.com/Guide/htmx-and-hyperscript.html) -- Add dynamic behavior with htmx
- [Tailwind CSS](https://ihp.digitallyinduced.com/Guide/tailwindcss.html) -- Set up Tailwind CSS for styling
- [Static Assets](https://ihp.digitallyinduced.com/Guide/assets.html) -- Manage CSS, JavaScript, and image assets
- [NPM Packages](https://ihp.digitallyinduced.com/Guide/npm.html) -- Use NPM packages in your IHP project
- [Server-Side Components](https://ihp.digitallyinduced.com/Guide/server-side-components.html) -- Build interactive components rendered on the server
- [Realtime SPAs](https://ihp.digitallyinduced.com/Guide/realtime-spas.html) -- Build single-page apps with real-time updates

### 5. Going Further

- [Relationships](https://ihp.digitallyinduced.com/Guide/relationships.html) -- Work with belongs-to, has-many, and many-to-many relations
- [QueryBuilder](https://ihp.digitallyinduced.com/Guide/querybuilder.html) -- Construct type-safe database queries
- [Typed SQL](https://ihp.digitallyinduced.com/Guide/typed-sql.html) -- Write raw SQL with type-checked results
- [Pagination](https://ihp.digitallyinduced.com/Guide/pagination.html) -- Paginate large result sets
- [WebSockets](https://ihp.digitallyinduced.com/Guide/websockets.html) -- Add real-time bidirectional communication
- [Stripe](https://ihp.digitallyinduced.com/Guide/stripe.html) -- Integrate Stripe for payments
- [OAuth](https://ihp.digitallyinduced.com/Guide/oauth.html) -- Add third-party login via OAuth providers
- [Passkeys & WebAuthn](https://ihp.digitallyinduced.com/Guide/passkeys.html) -- Enable passwordless authentication with passkeys
- [Internationalization](https://ihp.digitallyinduced.com/Guide/i18n.html) -- Add multi-language support to your app
- [Testing](https://ihp.digitallyinduced.com/Guide/testing.html) -- Write and run tests for your application
- [Package Management](https://ihp.digitallyinduced.com/Guide/package-management.html) -- Add Haskell packages and manage dependencies
- [Naming Conventions](https://ihp.digitallyinduced.com/Guide/naming-conventions.html) -- Understand IHP's naming conventions
- [Recipes](https://ihp.digitallyinduced.com/Guide/recipes.html) -- Solutions to common tasks and patterns

### 6. Production

- [Security](https://ihp.digitallyinduced.com/Guide/security.html) -- Understand IHP's built-in security protections and best practices
- [Deployment](https://ihp.digitallyinduced.com/Guide/deployment.html) -- Deploy your IHP application to production
- [Config](https://ihp.digitallyinduced.com/Guide/config.html) -- Configure framework settings and environment variables
- [Logging](https://ihp.digitallyinduced.com/Guide/logging.html) -- Set up structured logging
- [SEO](https://ihp.digitallyinduced.com/Guide/seo.html) -- Optimize your app for search engines
- [Debugging](https://ihp.digitallyinduced.com/Guide/debugging.html) -- Diagnose and fix issues in development and production
- [Troubleshooting](https://ihp.digitallyinduced.com/Guide/troubleshooting.html) -- Solve common problems and errors
- [Updating IHP](https://ihp.digitallyinduced.com/Guide/updating.html) -- Upgrade to newer IHP versions

## Feature Overview

-   **Fully managed development environment:** Works on any machine because all dependencies (including PostgreSQL) are managed using the Nix package manager. You only need to know a single command to start the app.
-   **Auto live reloading using virtual DOM in development mode:** Each code change automatically triggers the web page to refresh. The refresh uses a diff based patch to avoid resetting the page state. This means: Changes are reflected instantly.
-   **Build robust, type-safe applications:** With the power of Haskell your applications are going to be a lot more robust. Pretty much no runtime errors in production. You can refactor with confidence.
-   **Fast development environment:** While we use a compiled language, the built-in development server automatically reloads your code changes using the fastest way possible. Usually, changes are reflected in less than 50ms (a lot faster than your average Webpack setup).
-   **Faster production environment:** Production response times are around 30ms. With help of [instant click](http://instantclick.io/) it is faster than most single-page applications.
-   **Integrated development tooling:** You only need a text editor, everything else is taken care of.
-   **Scalable Application Architecture:** IHP is the result of building lots of real-world applications with Haskell.

## Professional Use

Before open-sourcing, IHP has already been used in production by [digitally induced](https://www.digitallyinduced.com/) since 2017. Therefore you can expect continuous support and development in the future.


### digitally induced Partners

The digitally induced Partners offer you professional IHP development, consulting and support. All partners have experience in working with IHP projects in production can help you build fast and well-architected projects.

If you're using IHP in a professional context, the partner network ensures that there's always someone that can take over your project if needed.

**Platinum Partners:**

- [Systor Vest](https://systorvest.no/)
- [Comhlan](http://comhlan.com/)
- [Zallocate Software](https://zallocate.com/)

You can find details on [the Partners page](https://ihp.digitallyinduced.com/Partners).

## Example Project

Take a look [at the example blog application project](https://github.com/digitallyinduced/ihp-blog-example-app) to get to see some code. Follow the documentation to build this application yourself in the section "Your First Project".

[Next: Installing IHP](https://ihp.digitallyinduced.com/Guide/installation.html)
