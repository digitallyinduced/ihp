module IHP.IDE.Data.View.ShowQuery where

import qualified Database.PostgreSQL.Simple as PG
import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.Layout

data ShowQueryView = ShowQueryView
    { queryResult :: Either PG.SqlError [[DynamicField]]
    , queryText :: Text
    }

instance View ShowQueryView where
    html ShowQueryView { .. } = [hsx|
        <div class="mx-2 pt-5">
            <div class="row no-gutters bg-white">
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows}
                </div>
            </div>
            {customQuery queryText}
        </div>
    |]
        where
            renderRows = case queryResult of
                Right rows -> [hsx|
                    <table class="table table-sm table-hover table-striped data-rows-table">
                        {tableHead rows}
                        {tableBody rows}
                    </table>
                |]
                Left sqlError -> [hsx|
                    <div class="alert alert-danger" role="alert">
                        <h4 class="alert-heading">SQL Error - {get #sqlExecStatus sqlError}</h4>
                        {showIfNotEmpty "Message" (get #sqlErrorMsg sqlError)}
                        {showIfNotEmpty "Details" (get #sqlErrorDetail sqlError)}
                        {showIfNotEmpty "Hint" (get #sqlErrorHint sqlError)}
                        {showIfNotEmpty "State" (get #sqlState sqlError)}
                    </div>
                |]

            tableHead rows = [hsx|<thead><tr>{forEach (columnNames rows) renderColumnHead}</tr></thead>|]
            renderColumnHead name = [hsx|<th>{name}</th>|]

            tableBody rows = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr>{forEach fields renderField}</tr>|]
            renderField DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}>{sqlValueToText fieldValue}</span></td>|]

            columnNames rows = maybe [] (map (get #fieldName)) (head rows)

            showIfNotEmpty :: Text -> ByteString -> Html
            showIfNotEmpty title = \case
                "" -> mempty
                text -> [hsx|<div><strong>{title}:</strong> {text}</div>|]
