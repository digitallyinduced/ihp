module IHP.IDE.Data.View.ShowQuery where

import qualified Database.PostgreSQL.Simple as PG
import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.Layout

data ShowQueryView = ShowQueryView
    { queryResult :: Maybe (Either PG.SqlError SqlConsoleResult)
    , queryText :: Text
    }

instance View ShowQueryView where
    html ShowQueryView { .. } = [hsx|
        <div class="h-100">
            {headerNav}
            <div class="container-fluid mt-2">
                <form method="GET" action={QueryAction} class="sql-repl">
                    <input type="hidden" name="query" value={queryText}/>

                    <div class="p-2 rounded my-2" style="background-color: #002B36; border: 1px solid #0B5163;">
                        <div class="query-editor" style="height:16px">{queryText}</div>
                    </div>

                    <button
                        class="btn btn-primary"
                        data-toggle="tooltip"
                        data-placement="right"
                        title="⌘ Enter"
                    >Run SQL Query</button>
                </form>
                <div class="mt-3">
                    {renderRows}
                </div>
            </div>
        </div>
    |]
        where
            renderRows = case queryResult of
                Just ((Right (SelectQueryResult []))) -> [hsx|
                    <div class="text-muted">
                        The query returned an empty result set.
                    </div>
                |]
                Just (Right (SelectQueryResult rows)) -> [hsx|
                    <table class="table table-sm table-hover table-striped data-rows-table">
                        {tableHead rows}
                        {tableBody rows}
                    </table>
                |]
                Just (Right (InsertOrUpdateResult count)) -> [hsx|
                    <div class="text-muted">
                        {count} {if count == 1 then "row" :: Text else "rows"} affected.
                    </div>
                |]
                Just (Left sqlError) -> [hsx|
                    <div class="alert alert-danger" role="alert">
                        <h4 class="alert-heading">SQL Error - {get #sqlExecStatus sqlError}</h4>
                        {showIfNotEmpty "Message" (get #sqlErrorMsg sqlError)}
                        {showIfNotEmpty "Details" (get #sqlErrorDetail sqlError)}
                        {showIfNotEmpty "Hint" (get #sqlErrorHint sqlError)}
                        {showIfNotEmpty "State" (get #sqlState sqlError)}
                    </div>
                |]
                Nothing -> mempty

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
