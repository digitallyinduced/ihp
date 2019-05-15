{-# LANGUAGE UndecidableInstances #-}
module Foundation.GenericController where

import Foundation.ControllerPrelude
import qualified Foundation.RouterSupport as Router
import Foundation.ModelSupport
import GHC.TypeLits
import Database.PostgreSQL.Simple.FromRow
import Foundation.ViewPrelude
import Foundation.ViewSupport
import Foundation.Controller.Context ()

instance {-# OVERLAPPABLE #-} (RestfulController controller, Router.Child controller ~ controller, Eq controller, model ~ GetModelById (RestfulControllerId controller), KnownSymbol (GetTableName model), FromRow model, Show model, Generic model, HasField "id" model id, Show id) => Controller controller () where
	action theAction | isIndexAction @controller theAction = do
		models <- query @model |> fetch
		renderHtml (indexView models)

	action otherwise = renderPlain "unsupported action"


data GenericControllerViewContext = GenericControllerViewContext
	{ layout :: Layout
	}
	deriving (Generic)

type Html' = HtmlWithContext GenericControllerViewContext

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


instance CreateViewContext GenericControllerViewContext where
	type ControllerContext GenericControllerViewContext = ()
	createViewContext = return GenericControllerViewContext { layout = renderLayout }

indexView :: forall idType model. (Generic model, HasField "id" model idType, Show idType, Show model) => [model] -> Html'
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
