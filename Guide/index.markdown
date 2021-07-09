# Getting Started With Integrated Haskell Platform

IHP is a full-stack framework focused on rapid application development while striving for robust code quality.

We believe that functional programming is the future of software development and want to make functional programming with Haskell and Nix available to anyone. We try to offer a solution that can be used by developers who have not worked with Haskell yet. IHP comes with everything you need to build great web applications with Haskell and Nix. We have made a lot of pragmatic decisions to get you started faster. This way you can just pick up Haskell along the way :-)

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

You can find details on [the Partners page](https://ihp.digitallyinduced.com/Partners).

## Example Project

Take a look [at the example blog application project](https://github.com/digitallyinduced/ihp-blog-example-app) to get to see some code. Follow the documentation to build this application yourself in the section "Your First Project".

[Next: Installing IHP](https://ihp.digitallyinduced.com/Guide/installation.html)
