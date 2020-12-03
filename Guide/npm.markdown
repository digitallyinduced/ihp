# Using NPM Packages

```toc
```

## Introduction

While IHP has a strong preference for server-side rendering sometimes there is no way around building a little bit of JavaScript. This guide will help you understand the best-practices of accessing the JavaScript ecosystem with IHP.

## Adding NodeJS to a project

To access the JavaScript ecosystem we need to add NodeJS to our project. You should also follow this step if you have NodeJS already installed on your system. Installing the NodeJS version via nix allows all people working on your project to get the exact NodeJS version you're using.

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

After that, you have `node` and `npm` available in your project.

While nix can theoretically also install NPM modules, it's easier to just use NPM directly. Otherwise, you will spend lots of time with nix-related issues. Because `nodejs` and `npm` are managed by nix we still have reproducible builds. Every developer on your team will use the same NodeJS and NPM version.

### NPM Init

Before we can install dependencies, we need to generate a `package.json`:

```bash
npm init
```

Add the `package.json` and the `package.lock` to your git repository.

### Installing NPM modules

Now we can just install node modules as required by calling `npm install`:

```bash
npm add tailwindcss postcss autoprefixer
```
