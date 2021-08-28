module IHP.IDE.Data.View.EditValue where

import IHP.ViewPrelude
import IHP.IDE.ToolServer.Types
import IHP.IDE.Data.View.ShowDatabase
import IHP.IDE.Data.View.Layout
import Data.Maybe

data EditValueView = EditValueView
    { tableNames :: [Text]
    , tableName :: Text
    , rows :: [[DynamicField]]
    , targetName :: Text
    , targetId :: Text
    }

instance View EditValueView where
    html EditValueView { .. } = [hsx|
        <div class="mx-2 pt-5">
            <div class="row no-gutters bg-white">
                {renderTableSelector tableNames tableName}
                <div class="col" style="overflow: scroll; max-height: 80vh">
                    {renderRows rows tableBody tableName}
                    <div class="text-muted context-menu-notice">Press Enter to save your changes or click somewhere else to discard them.</div>
                </div>
            </div>
            {customQuery ""}
        </div>
        {script}
    |]
        where

            tableBody = [hsx|<tbody>{forEach rows renderRow}</tbody>|]
            renderRow fields = [hsx|<tr oncontextmenu={"showContextMenu('" <> contextMenuId <> "');"}>{forEach fields (renderField id)}
            </tr>
            <div class="custom-menu menu-for-column shadow backdrop-blur" id={contextMenuId}>
                <a href={EditRowAction tableName id}>Edit Row</a>
                <a href={DeleteEntryAction id tableName} class="js-delete">Delete Row</a>
                <div></div>
                <a href={NewRowAction tableName}>Add Row</a>
            </div>|]
                where
                    contextMenuId = "context-menu-column-" <> tshow id
                    id = (cs (fromMaybe "" (get #fieldValue (fromJust (headMay fields)))))

            renderField id DynamicField { .. } | (tshow targetName) == (tshow fieldName) && targetId == id = [hsx|<td>
                <form id="fieldForm" method="POST" action={UpdateValueAction}>
                    <input id="editField" autofocus="autofocus" type="text" name="targetValue" value={fromMaybe "" fieldValue}/>
                    <input id="inputField" type="hidden" name="tableName" value={tableName}/>
                    <input type="hidden" name="id" value={id}/>
                    <input type="hidden" name="targetName" value={targetName}/>
                </form></td>|]
            renderField id DynamicField { .. } | fieldName == "id" = [hsx|<td><span data-fieldname={fieldName}><a class="no-link border rounded p-1" href={EditRowValueAction tableName (cs fieldName) id}>{renderId (sqlValueToText fieldValue)}</a></span></td>|]
            renderField id DynamicField { .. } = [hsx|<td><span data-fieldname={fieldName}><a class="no-link" href={EditRowValueAction tableName (cs fieldName) id}>{sqlValueToText fieldValue}</a></span></td>|]
            script = preEscapedToHtml [plain|
                <script>
                    onClickHandler = () => {
                        window.location = "#{pathTo (ShowTableRowsAction tableName)}";
                    }
                    document.addEventListener('click', onClickHandler);
                    var editField = document.getElementById("editField");
                    editField.addEventListener('click', function (event) {event.stopPropagation();});


                    var input = document.getElementById("inputField");
                    input.addEventListener("keyup", function(event) {
                    if (event.keyCode === 13) {
                        var form = document.getElementById("fieldForm");
                        window.submit(form);
                    }
                    });
                </script>
            |]