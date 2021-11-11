module IHP.IDE.Data.View.Layout
    ( customQuery
    , tableHead
    , renderColumnHead
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
    ) where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import qualified Data.Text as Text

customQuery :: Text -> Html
customQuery input = [hsx|<div class="p-2 rounded mt-2" style="background-color: #002B36;"><div id="queryInput" style="height:16px">{input}</div></div>|]

tableHead :: [[DynamicField]] -> Text -> Html
tableHead rows tableName =
    [hsx|<thead><tr>{forEach (columnNames rows) renderColumnHead}</tr></thead>|]
    where
        columnNames rows = map (get #fieldName) (fromMaybe [] (head rows))
renderColumnHead name = [hsx|<th>{name}</th>|]

renderRows :: [[DynamicField]] -> Html -> Text -> Html
renderRows rows body tableName = [hsx|
    <table class="table table-sm table-hover table-striped data-rows-table">
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
