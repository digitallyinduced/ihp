# Great Error Message

IHP error messages should be clear, actionable, and practical. A great error message tells the user what went wrong and what the user needs to do to fix the problem. It should direct the user to the solution most likely needed.

In case the error messages are produced by GHC, [we should use troubleshooting helpers in the `StatusServer` to show actionable steps](https://github.com/digitallyinduced/ihp/blob/master/IHP/IDE/StatusServer.hs#L281) for a solution. In case a Haskell exception is thrown at runtime, [we can use the `ErrorController` to explain the error message in the IHP context](https://github.com/digitallyinduced/ihp/blob/master/IHP/ErrorController.hs#L125).

## Examples

Here are some examples of improved error messages.

**Unclear error message:**

```
AutoRoute: Failed parsing UUID
```

**[Improved:](https://github.com/digitallyinduced/ihp/pull/334)**

```
AutoRoute: Failed parsing argument as UUID. You most likely are trying to use AutoRoute with a non-UUID attribute. See https://ihp.digitallyinduced.com/Guide/routing.html#parameter-types for details on how to configure this.
```
