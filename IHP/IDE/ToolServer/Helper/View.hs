module IHP.IDE.ToolServer.Helper.View where

import IHP.ViewPrelude

addIcon :: Html
addIcon = preEscapedToHtml [plain|<svg xmlns="http://www.w3.org/2000/svg" height="1rem" viewBox="0 0 24 24" fill="currentColor"><path d="M0 0h24v24H0z" fill="none"/><path d="M19 13h-6v6h-2v-6H5v-2h6V5h2v6h6v2z"/></svg>|]

editIcon :: Html
editIcon = preEscapedToHtml [plain|
<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" height="1rem" width="1rem" fill="currentColor"><path d="M0 0h24v24H0V0z" fill="none"/><path d="M14.06 9.02l.92.92L5.92 19H5v-.92l9.06-9.06M17.66 3c-.25 0-.51.1-.7.29l-1.83 1.83 3.75 3.75 1.83-1.83c.39-.39.39-1.02 0-1.41l-2.34-2.34c-.2-.2-.45-.29-.71-.29zm-3.6 3.19L3 17.25V21h3.75L17.81 9.94l-3.75-3.75z"/></svg>
|]

deleteIcon :: Html
deleteIcon = preEscapedToHtml [plain|
<svg xmlns="http://www.w3.org/2000/svg" height="1rem" viewBox="0 0 24 24" width="1rem" fill="currentColor"><path d="M0 0h24v24H0V0z" fill="none"/><path d="M16 9v10H8V9h8m-1.5-6h-5l-1 1H5v2h14V4h-3.5l-1-1zM18 7H6v12c0 1.1.9 2 2 2h8c1.1 0 2-.9 2-2V7z"/></svg>
|]
