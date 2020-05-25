# Getting Started With Integrated Haskell Platform
IHP is a full stack framework focused on rapid application development while striving for robust code quality.

We believe that functional programing is the future of software development and want to make functional programing with haskell and nix available to anyone. We try to offer a solution which can be used by developers who have not worked with haskell yet. IHP comes with everything you need to build great web applications with haskell and nix. We have made a lot of pragmatic decision to get you started faster. This way you can just pick up haskell along the way :-)

##### Feature Overview

- **Fully managed dev environment:** Works on any machine because all dependencies (including PostgreSQL) are managed using the nix package manager. You only need to know a single command to start the app.
- **Auto live reload using virtual dom in dev mode:** Each code change automatically triggers the web page to refresh. The refresh uses a diff based patch to avoid resetting the page state. This means: Changes are reflected instantly.
- **Build robust, type-safe applications:** With the power of haskell your application are going to be a lot more robust. Pretty much no runtime errors in production. You can refactor with confidence.
- **Fast dev enviroment:** While we use a compiled language, the built-in dev server automatically reloads your code changes using the fastest way possible. Usually changes are reflected in less than 50ms (alot faster than your average webpack setup).
- **Faster production environment:** Production response times around 30ms. With help of [instant click](http://instantclick.io/) it is faster than most single page applications.
- **Integrated dev tooling:** You only need a text editor, everything else is taken care of.
- **Scalable Application Architecture:** IHP is the result of building lots of real world applications with haskell.
