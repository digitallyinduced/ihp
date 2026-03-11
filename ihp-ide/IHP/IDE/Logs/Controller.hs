module IHP.IDE.Logs.Controller where

import IHP.ControllerPrelude
import IHP.IDE.ToolServer.Helper.Controller
import IHP.IDE.ToolServer.Types
import IHP.IDE.Logs.View.Logs
import qualified Data.ByteString.Char8 as ByteString
import qualified IHP.EnvVar as EnvVar
import qualified System.Directory as Directory
import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

instance Controller LogsController where
    action AppLogsAction = do
        toolServerApp <- fromContext @ToolServerApplication

        standardOutput <- cs . ByteString.unlines . reverse <$> readIORef toolServerApp.appStandardOutput
        errorOutput <- cs . ByteString.unlines . reverse <$> readIORef toolServerApp.appErrorOutput

        services <- discoverServices
        let activeService = "app"

        render LogsView { .. }

    action PostgresLogsAction = do
        pgdata <- EnvVar.env @String "PGDATA"
        let logFile = pgdata <> "/log/postgresql.log"

        logExists <- Directory.doesFileExist logFile
        standardOutput <- if logExists
            then cs <$> ByteString.readFile logFile
            else pure ("Postgres log file not found" :: ByteString)
        let errorOutput = "" :: ByteString

        services <- discoverServices
        let activeService = "postgres"

        render LogsView { .. }

    action ServiceLogsAction { serviceName } = do
        standardOutput <- cs <$> getServiceLogs serviceName
        let errorOutput = "" :: ByteString

        services <- discoverServices
        let activeService = serviceName

        render LogsView { .. }

    action OpenEditorAction = do
        let path = param @Text "path"
        let line = paramOrDefault @Int 0 "line"
        let col = paramOrDefault @Int 0 "col"
        openEditor path line col

        renderPlain ""

-- | Discover devenv services by checking process-compose config or Procfile.
-- Returns service names excluding "ihp" and "postgres" (which have dedicated tabs).
discoverServices :: IO [Text]
discoverServices = do
    result <- Exception.try @Exception.SomeException do
        -- Try process-compose first (PC_CONFIG_FILES env var)
        pcConfig <- EnvVar.envOrNothing "PC_CONFIG_FILES" :: IO (Maybe String)
        case pcConfig of
            Just configPath -> discoverFromProcessCompose configPath
            Nothing -> do
                -- Try Procfile
                procfileExists <- Directory.doesFileExist "Procfile"
                if procfileExists
                    then discoverFromProcfile "Procfile"
                    else pure []
    case result of
        Right services -> pure services
        Left _ -> pure []

-- | Parse process names from a process-compose YAML config file.
-- Looks for lines matching "^  <word>:" under the "processes:" key.
discoverFromProcessCompose :: String -> IO [Text]
discoverFromProcessCompose configPath = do
    -- PC_CONFIG_FILES can contain multiple paths separated by ","
    let firstPath = takeWhile (/= ',') configPath
    exists <- Directory.doesFileExist firstPath
    if exists
        then do
            content <- Text.IO.readFile firstPath
            let allLines = Text.lines content
            let processNames = extractProcessComposeNames allLines
            pure (filterBuiltinServices processNames)
        else pure []

-- | Extract process names from process-compose YAML.
-- Finds the "processes:" section and extracts direct child keys only.
extractProcessComposeNames :: [Text] -> [Text]
extractProcessComposeNames ls = go Nothing ls
    where
        go _ [] = []
        go state (l:rest)
            -- Start of processes section
            | Text.stripEnd l == "processes:" = go (Just Nothing) rest
            -- Not yet in processes section
            | Nothing <- state = go Nothing rest
            -- In processes section: top-level key means we left it
            | not (Text.null l), not (Text.isPrefixOf " " l), not (Text.isPrefixOf "\t" l) = []
            -- In processes section: try to parse a process name at the right indent
            | Just mIndent <- state
            , Just (indent, name) <- parseIndentedKey l
            = case mIndent of
                -- First process name: establishes the indent level
                Nothing -> name : go (Just (Just indent)) rest
                -- Subsequent: must match the established indent
                Just expectedIndent
                    | indent == expectedIndent -> name : go state rest
                    | indent < expectedIndent -> [] -- left the section (shouldn't happen in valid YAML)
                    | otherwise -> go state rest -- deeper nesting, skip
            | otherwise = go state rest

        parseIndentedKey line =
            let indent = Text.length (Text.takeWhile (== ' ') line)
                stripped = Text.strip line
            in if indent == 0 || Text.null stripped || Text.isPrefixOf "#" stripped
                then Nothing
                else case Text.breakOn ":" stripped of
                    (name, rest')
                        | not (Text.null rest')
                        , not (Text.null name)
                        , Text.all isNameChar name
                        -> Just (indent, name)
                    _ -> Nothing

        isNameChar c = c == '_' || c == '-' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Parse service names from a Procfile (format: "name: command").
discoverFromProcfile :: String -> IO [Text]
discoverFromProcfile path = do
    content <- Text.IO.readFile path
    let names = mapMaybe parseProcfileLine (Text.lines content)
    pure (filterBuiltinServices names)
    where
        parseProcfileLine line =
            let stripped = Text.strip line
            in if Text.null stripped || Text.isPrefixOf "#" stripped
                then Nothing
                else case Text.breakOn ":" stripped of
                    (name, rest')
                        | not (Text.null rest')
                        , not (Text.null name)
                        -> Just (Text.strip name)
                    _ -> Nothing

-- | Filter out services that already have dedicated tabs.
filterBuiltinServices :: [Text] -> [Text]
filterBuiltinServices = filter (\name -> Text.toLower name /= "ihp" && Text.toLower name /= "postgres")

-- | Read logs for a specific devenv service from the process manager log file.
-- Filters lines by the "serviceName |" prefix pattern.
getServiceLogs :: Text -> IO ByteString
getServiceLogs serviceName = do
    result <- Exception.try @Exception.SomeException do
        logFile <- findProcessManagerLogFile
        case logFile of
            Just path -> do
                exists <- Directory.doesFileExist path
                if exists
                    then do
                        content <- Text.IO.readFile path
                        let allLines = Text.lines content
                        let filtered = filterServiceLines serviceName allLines
                        let limited = takeLast 10000 filtered
                        pure (cs (Text.unlines limited))
                    else pure ("Log file not found: " <> cs path)
            Nothing -> pure "No process manager log file found"
    case result of
        Right output -> pure output
        Left e -> pure ("Error reading logs: " <> cs (show e))

-- | Find the process manager log file path.
findProcessManagerLogFile :: IO (Maybe String)
findProcessManagerLogFile = do
    -- Try DEVENV_STATE first
    devenvState <- EnvVar.envOrNothing "DEVENV_STATE" :: IO (Maybe String)
    case devenvState of
        Just state -> do
            -- Check process-compose log
            let pcLog = state <> "/process-compose/process-compose.log"
            pcExists <- Directory.doesFileExist pcLog
            if pcExists
                then pure (Just pcLog)
                else do
                    -- Check overmind log
                    let omLog = state <> "/overmind.log"
                    omExists <- Directory.doesFileExist omLog
                    if omExists
                        then pure (Just omLog)
                        else pure Nothing
        Nothing -> do
            -- Fallback: check .devenv/state
            let pcLog = ".devenv/state/process-compose/process-compose.log"
            pcExists <- Directory.doesFileExist pcLog
            if pcExists
                then pure (Just pcLog)
                else do
                    let omLog = ".devenv/state/overmind.log"
                    omExists <- Directory.doesFileExist omLog
                    if omExists
                        then pure (Just omLog)
                        else pure Nothing

-- | Filter log lines that belong to a specific service.
-- Matches patterns like "serviceName  |" or "serviceName |" at the start of each line.
filterServiceLines :: Text -> [Text] -> [Text]
filterServiceLines name = mapMaybe extractLine
    where
        -- Process-compose uses "name           | content" (padded with spaces)
        -- Overmind uses "name | content"
        extractLine line =
            let stripped = Text.stripStart line
            in case Text.breakOn "|" stripped of
                (prefix, rest')
                    | not (Text.null rest')
                    , Text.strip prefix == name
                    -> Just (Text.drop 1 rest') -- Drop the "|" character
                _ -> Nothing

-- | Take the last n elements from a list.
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs
