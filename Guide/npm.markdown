# Using NPM Packages

```toc
```

## Introduction

While IHP has a strong preference for serverside rendering sometimes there is no way around building a little bit of Javascript. This guide will help you understand the best-practises of accessing the JS ecosystem with IHP.

## Adding NodeJS to a project

To access the JS ecosystem we need to add nodejs to our project. You should also follow this step if you have nodejs already installed on your system. Installing the nodejs version via nix allows all people working on your project to get the exact same nodejs version as you're using.

For that open your projects `default.nix` and add `nodejs` to `otherDeps`:

```nix
        otherDeps = p: with p; [
            # Native dependencies, e.g. imagemagick
            nodejs
        ];
```

Now you need to rebuild your local dev environment:

```bash
nix-shell --run 'make -B .envrc'
```

After that you have `node` and `npm` available in your project.

While nix can theoretically also install NPM modules, it's easier to just use npm directly. Oterhwise you will spend lots of time on fighting nix-related issues. Because `nodejs` and `npm` are itself managed by nix we still have reproducable builds. Every developer on your team will use the exact same nodejs and NPM version.

### NPM Init

Before we can install dependencies, we need generate a `package.json`:

```bash
npm init
```

Add the `package.json` and the `package.lock` to your git repository.


### Installing NPM modules

Now we can just install node modules as required by calling `npm install`:

```bash
npm add tailwindcss postcss autoprefixer
```