# ihp-welcome

This package provides the default welcome page controller for new IHP applications.

## Purpose

The `IHP.Welcome.Controller` module provides a simple `WelcomeAction` controller that displays a welcome page when you first create an IHP application. This is useful for getting started with IHP, but most production applications will remove this controller and replace it with their own start page.

## Usage

In a new IHP project, you can import and use the welcome controller in your `Web/FrontController.hs`:

```haskell
import IHP.Welcome.Controller

instance FrontController WebApplication where
    controllers =
        [ startPage WelcomeAction
        -- ... other controllers
        ]
```

## Removing the Welcome Controller

For production applications, you typically want to remove the welcome controller and replace it with your own start page:

1. Remove the `ihp-welcome` dependency from your project's `.cabal` file
2. Remove the `import IHP.Welcome.Controller` line from your `Web/FrontController.hs`
3. Replace `startPage WelcomeAction` with your own start page action

For example:

```haskell
instance FrontController WebApplication where
    controllers =
        [ startPage ProjectsAction  -- Your own start page
        -- ... other controllers
        ]
```

## Package Structure

This is a separate package from the main `ihp` package to allow production applications to exclude it and reduce their dependency footprint.
