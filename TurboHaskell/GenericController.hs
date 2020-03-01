{-# LANGUAGE UndecidableInstances #-}
module TurboHaskell.GenericController where

import TurboHaskell.ControllerPrelude
import qualified TurboHaskell.RouterSupport as Router
import TurboHaskell.ModelSupport
import GHC.TypeLits
import Database.PostgreSQL.Simple.FromRow
import TurboHaskell.ViewPrelude hiding (query, fetch, param)
import TurboHaskell.ViewSupport
import qualified Text.Blaze.Html5 as Html5

instance {-# OVERLAPPABLE #-} (AutoRoute controller, Eq controller, KnownSymbol (GetTableName model), FromRow model, Show model, Generic model, HasField "id" model id, Show id, Show controller) => Controller controller where
    action theAction | isIndexAction @controller theAction = do
        models <- query @model |> fetch
        respondHtml (renderLayout (indexView models))

    action otherwise = renderPlain "unsupported action"


data GenericControllerViewContext = GenericControllerViewContext
    { layout :: Layout
    }
    deriving (Generic)

renderLayout :: Layout
renderLayout view = [hsx|
<html lang="en">
  <head>
    <meta charset="utf-8" />   
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />


    <link rel="stylesheet" href="https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" integrity="sha384-ggOyR0iXCbMQv3Xipma34MD+dH/1fQ784/j6cY/iJTQUOhcWr7x9JvoRxT2MZw1T" crossorigin="anonymous" />

    <title>Hello, world!</title>
  </head>
  <body>{view}</body>
</html>
|]


indexView :: forall idType model. (Generic model, HasField "id" model idType, Show idType, Show model) => [model] -> Html5.Html
indexView models = [hsx|
    <div class="container">
        <h1>Models</h1>

        <table class="table">
            <thead>
                <tr>
                    <th>Id</th>
                    <th>Model</th>
                </tr>
            </thead>
            <tbody>
                {forM_ models renderRow}
            </tbody>
        </table>
    </div>
    |]
        where
            renderRow model = [hsx|
                <tr>
                    <td>{tshow idField}</td>
                    <td><code>{tshow model}</code></td>
                </tr>
            |]
                where
                    idField :: idType = getField @"id" model
