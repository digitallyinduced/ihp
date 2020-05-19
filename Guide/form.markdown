# Form

```toc
```

## Introduction

## Select Inputs

You can use the `selectField` helper for select inputs:

```haskell
formFor project [hsx|
    {selectField #userId users}
|]
```
In the example above the variable `users` contains all the possible option values for the select.

You also need to define a instance `CanSelect User`:
```haskell
instance CanSelect User where
    -- Here we specify that the <option>-value should contain a UserId
    type SelectValue User = UserId
    -- Here we specify how to transform the model into <option>-value
    selectValue = get #id
    -- And here we specify the <option>-text
    selectLabel = get #name
```

Given the above example, the rendered form will look like this:
```html
-- Assuming: users = [User { id = 1, name = "Marc" }, User { id = 2, name = "Andreas" }]
<form ...>
    <select name="user_id">
        <option value="1">Marc</option>
        <option value="2">Andreas</option>
    </select>
</form>
```


