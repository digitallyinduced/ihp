module IHP.IDE.SchemaDesigner.View.Schema.SchemaUpdateFailed where

import IHP.ViewPrelude
import IHP.IDE.SchemaDesigner.Types
import IHP.IDE.ToolServer.Types
import IHP.IDE.ToolServer.Layout
import IHP.IDE.SchemaDesigner.View.Layout
import System.Exit
import qualified Data.Text as Text

data SchemaUpdateFailedView = SchemaUpdateFailedView
    { output :: Text
    , exitCode :: ExitCode
    }

instance View SchemaUpdateFailedView where
    html SchemaUpdateFailedView { .. } = renderModal Modal
                { modalTitle = "Open your Fixtures.sql and apply required changes to fix this error. After that try again."
                , modalCloseUrl = pathTo TablesAction
                , modalFooter = Nothing
                , modalContent = [hsx|
                        <div class={classes ["schema-update-failed", ("sql-error", isSqlError)]}>
                            {forEach errorMessages renderError}
                            {outputLines |> map renderLine |> mconcat}
                        </div>
                    |]
                }

        where
            -- | E.g. the make db succeeded but there an sql error inside the output
            isSqlError = exitCode == ExitSuccess

            outputLines :: [Text]
            outputLines = Text.lines output

            errorMessages :: [Text]
            errorMessages = 
                    zip [0..] outputLines
                    |> filter (\(i, line) -> "ERROR" `Text.isInfixOf` line)
                    |> map (\(i, line) ->
                            let nextLine :: Text = if i < length outputLines then outputLines !! (i + 1) else ""
                            in
                                if "DETAIL" `isPrefixOf` nextLine
                                    then (i, line <> "\n" <> nextLine)
                                    else (i, line)
                        )
                    |> map snd

            renderError message = [hsx|<div class="error">{nl2br message}</div>|]

            renderLine :: Text -> Html
            renderLine line = [hsx|<div>{line}</div>|]