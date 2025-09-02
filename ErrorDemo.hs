{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module ErrorDemo where

import IHP.HSX.QQ
import IHP.HSX.ToHtml

-- Undefined variable error
undefinedVarDemo = [hsx|
    <div>{undefinedVariable}</div>
|]

-- Type error in toHtml
data MyCustomType = MyCustomType
typeErrorDemo = [hsx|
    <div>{MyCustomType}</div>
|]

-- Missing closing tag
missingTagDemo = [hsx|
    <div>
        <span>Hello World
    </div>
|]

-- Invalid tag name
invalidTagDemo = [hsx|
    <myinvalidtag>Hello</myinvalidtag>
|] 