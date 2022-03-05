module IHP.IDE.Data.View.Layout
    ( customQuery
    , tableHead
    , renderRows
    , sqlValueToText
    , renderId
    , isBoolField
    , isSqlFunction
    , isSqlFunction_
    , fillField
    , getColDefaultValue
    , renderRowValue
    , renderDefaultWithoutType
    , isBooleanParam
    , headerNav
    ) where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Routes
import qualified Data.Text as Text

customQuery :: Text -> Html
customQuery input = [hsx|<div class="p-2 rounded mt-2" style="background-color: #002B36;"><div id="queryInput" style="height:16px">{input}</div></div>|]

tableHead :: [[DynamicField]] -> Text -> Html
tableHead rows tableName =
    [hsx|
        <thead>
            <tr>
                {forEach (columnNames rows) renderColumnHead}
                <th>
                    <div class="d-flex">
                        <a
                            href={NewRowAction tableName}
                            class="btn btn-link btn-add"
                            data-toggle="tooltip"
                            data-placement="bottom"
                            title={"Add " <> tableNameToModelName tableName}
                        >{addIcon}</a>
                    </div>
                </th>
            </tr>
        </thead>
    |]
    where
        columnNames rows = map (get #fieldName) (fromMaybe [] (head rows))
        renderColumnHead name = [hsx|<th>{name}</th>|]

renderRows :: [[DynamicField]] -> Html -> Text -> Html
renderRows rows body tableName = [hsx|
    <table class="table table-sm table-hover data-rows-table">
        {tableHead rows tableName}
        {body}
    </table>
|]

sqlValueToText :: Maybe ByteString -> Text
sqlValueToText (Just value) = cs value
sqlValueToText Nothing = "NULL"

renderId id = take 4 (cs id) <> ".." <> reverse (take 4 (reverse (cs id)))

isBoolField fieldName tableCols = case (find (\c -> get #columnName c == (cs fieldName)) tableCols) of
    Just columnDef -> (get #columnType columnDef) == "boolean"
    Nothing -> False

isSqlFunction :: Text -> Bool
isSqlFunction text = text `elem`
    [ "uuid_generate_v4()"
    , "NOW()"
    , "NULL"]

isSqlFunction_ :: ByteString -> Bool
isSqlFunction_ text = text `elem`
    [ "uuid_generate_v4()"
    , "NOW()"
    , "NULL"]

fillField col value isBoolField = "fillField('" <> get #columnName col <> "', '" <> value <> "'," <> isBoolField <> ");"

getColDefaultValue :: ColumnDefinition -> Text
getColDefaultValue ColumnDefinition { columnDefault, isNullable } = case columnDefault of
        Just value -> value
        Nothing -> if isNullable
            then "NULL"
            else ""

renderRowValue :: Maybe ByteString -> Text
renderRowValue (Just value) = "'" <> cs value <> "'"
renderRowValue Nothing = "NULL"

renderDefaultWithoutType :: Text -> Text
renderDefaultWithoutType "" = ""
renderDefaultWithoutType input = case length (Text.splitOn "'" input) of
        3 -> (Text.splitOn "'" input) !! 1
        _ -> input

isBooleanParam :: Bool -> ColumnDefinition -> Html
isBooleanParam isBool def = [hsx|
<input
    type="hidden"
    name={get #columnName def <> "-isBoolean"}
    value={inputValue isBool}
    />
|]


headerNav :: Html
headerNav = [hsx|
    <div class="view-selector">
        <div class="container-fluid">
            <a href={ShowDatabaseAction} class={classes [("active", databaseActive)]}>
                Database
            </a>

            <a href={NewQueryAction} class={classes [("active", sqlActive)]}>
                SQL
            </a>
        </div>
    </div>
|]
    where
        databaseActive :: Bool
        databaseActive = isActiveController @DataController

        sqlActive :: Bool
        sqlActive = False


addIcon :: Html
addIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" height="1rem" viewBox="0 0 24 24" fill="currentColor"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 13h-6v6h-2v-6H5v-2h6V5h2v6h6v2z"/></svg>|]