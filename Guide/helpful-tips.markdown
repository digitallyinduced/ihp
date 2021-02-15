# Helpful Tips

## IHP Flags

-   `export IHP_BROWSER=echo` - Don't open a browser when server starts
-   `export IHP_EDITOR="code --goto"` - Set the editor, when you get error messages during development, you could click on them to automatically go to the file/line in the editor.

## Tell GHC(Haskell Compiler) To Infer Constraints And Implicit Parameters

Let's say you are working with a controller `ApplicationsAction` and most actions have similar access control:

```haskell
    action NewApplicationAction { jobPositionId } = do
        jobPosition <- fetch jobPositionId

        -- Access Control
        jobPositions <- currentCompanyJobPositions
        accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))

        ...

    action UpdateApplicationAction { applicationId } = do
        application <- fetch applicationId

        -- Access Control
        jobPositions <- currentCompanyJobPositions
        accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))

        ...
```

We could start by refactoring the access control logic into a function:

```haskell
accessDeniedUnlessJobPositionAllowed jobPosition = do
    jobPositions <- currentCompanyJobPositions
    accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
```

And then add a type declaration:

```haskell
accessDeniedUnlessJobPositionAllowed :: JobPosition -> IO ()
accessDeniedUnlessJobPositionAllowed jobPosition = do
    jobPositions <- currentCompanyJobPositions
    accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
```

However, GHC will give us an error message stating:

```
...

Application/Helper/Controller.hs:51:21: error:
    * Unbound implicit parameter (?context::ControllerContext)
        arising from a use of `currentCompanyJobPositions'
    * In a stmt of a 'do' block:
        jobPositions <- currentCompanyJobPositions
      In the expression:
        do jobPositions <- currentCompanyJobPositions
           accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
      In an equation for `accessDeniedUnlessJobPositionAllowed':
          accessDeniedUnlessJobPositionAllowed jobPosition
            = do jobPositions <- currentCompanyJobPositions
                 accessDeniedUnless (get #id jobPosition `elem` (ids jobPositions))
   |
51 |     jobPositions <- currentCompanyJobPositions
   |
```

We could explicitly add the `?context::ControllerContext` implicit:

```haskell
accessDeniedUnlessJobPositionAllowed :: (?context::ControllerContext) => JobPosition -> IO ()
```

Writing out implicit parameters, and other type constrains could become messy and/or irritating, so we could tell GHC to infer it:

```haskell
accessDeniedUnlessJobPositionAllowed :: _ => JobPosition -> IO ()
```
