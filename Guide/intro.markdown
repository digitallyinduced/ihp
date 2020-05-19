# Getting Started With Turbo Haskell
Turbo Haskell is a full stack framework focused on rapid application development while striving for robust code quality.

This guide covers everything you need to ship software with IHP. 

##### Feature Overview

- **Fully managed dev environment:** Works on any machine because all dependencies (including PostgreSQL) are managed using the nix package manager.
- **Bootstrap 4 Out of the box:** The default layout already integrates bootstrap 4.
- **Auto live reload using virtual dom in dev mode:** Each code change changes your local page to refresh. The refresh uses a diff based patch to avoid resetting the page state.
- **Build robust applications:** With the power of haskell your application are going to be a lot more robust. Pretty much no runtime errors in production.
- **Fast dev enviroment:** While we use a compiled language, the built-in dev server automatically reloads your code changes using the fastest way possible. Usually changes are reflected in less than 100ms (alot faster than your average webpack setup).
- **Very fast production environment:** Production response times around 30ms. Using [instant click](http://instantclick.io/) it can be faster than your average SPA.