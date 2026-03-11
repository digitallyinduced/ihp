-- | Service discovery and log reading for devenv process managers.
--
-- Supports process-compose (via @$PC_CONFIG_FILES@) and overmind (via Procfile).
-- Log lines are read from the process manager's combined log file and filtered
-- by the @"serviceName |"@ prefix each manager prepends.
module IHP.IDE.Logs.ServiceLog
    ( discoverServices
    , getServiceLogs
    -- * Pure helpers (exported for testing)
    , extractProcessComposeNames
    , filterServiceLines
    , filterBuiltinServices
    ) where

import IHP.Prelude
import qualified IHP.EnvVar as EnvVar
import qualified System.Directory as Directory
import qualified Control.Exception.Safe as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO

-- | Discover devenv services by checking process-compose config or Procfile.
-- Returns service names excluding "ihp" and "postgres" (which have dedicated tabs).
discoverServices :: IO [Text]
discoverServices = do
    result <- Exception.tryAny discoverServices'
    case result of
        Right services -> pure services
        Left _ -> pure []
    where
        discoverServices' = do
            pcConfig <- EnvVar.envOrNothing "PC_CONFIG_FILES" :: IO (Maybe String)
            case pcConfig of
                Just configPath -> discoverFromProcessCompose configPath
                Nothing -> do
                    procfileExists <- Directory.doesFileExist "Procfile"
                    if procfileExists
                        then discoverFromProcfile "Procfile"
                        else pure []

-- | Read logs for a specific devenv service from the process manager log file.
-- Filters lines by the @"serviceName |"@ prefix pattern and returns the last 10 000 lines.
getServiceLogs :: Text -> IO ByteString
getServiceLogs serviceName = do
    result <- Exception.tryAny do
        logFile <- findProcessManagerLogFile
        case logFile of
            Just path -> do
                exists <- Directory.doesFileExist path
                if exists
                    then do
                        content <- Text.IO.readFile path
                        let filtered = filterServiceLines serviceName (Text.lines content)
                        let limited = takeLast 10000 filtered
                        pure (cs (Text.unlines limited))
                    else pure ("Log file not found: " <> cs path)
            Nothing -> pure "No process manager log file found"
    case result of
        Right output -> pure output
        Left e -> pure ("Error reading logs: " <> cs (show e))

------------------------------------------------------------------------
-- Service discovery
------------------------------------------------------------------------

discoverFromProcessCompose :: String -> IO [Text]
discoverFromProcessCompose configPath = do
    -- PC_CONFIG_FILES can contain multiple paths separated by ","
    let firstPath = takeWhile (/= ',') configPath
    exists <- Directory.doesFileExist firstPath
    if exists
        then do
            content <- Text.IO.readFile firstPath
            pure (filterBuiltinServices (extractProcessComposeNames (Text.lines content)))
        else pure []

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
                        | not (Text.null rest'), not (Text.null name)
                        -> Just (Text.strip name)
                    _ -> Nothing

------------------------------------------------------------------------
-- Log file location
------------------------------------------------------------------------

findProcessManagerLogFile :: IO (Maybe String)
findProcessManagerLogFile = do
    devenvState <- EnvVar.envOrNothing "DEVENV_STATE" :: IO (Maybe String)
    let baseDirs = maybe [".devenv/state"] (\s -> [s, ".devenv/state"]) devenvState
    findFirst
        [ dir <> suffix
        | dir <- baseDirs
        , suffix <- ["/process-compose/process-compose.log", "/overmind.log"]
        ]
    where
        findFirst [] = pure Nothing
        findFirst (p:ps) = do
            exists <- Directory.doesFileExist p
            if exists then pure (Just p) else findFirst ps

------------------------------------------------------------------------
-- Pure helpers
------------------------------------------------------------------------

-- | Extract process names from process-compose YAML.
-- Finds the @processes:@ section and extracts direct child keys only,
-- ignoring deeper-nested keys like @command:@ or @depends_on:@.
extractProcessComposeNames :: [Text] -> [Text]
extractProcessComposeNames ls = go Nothing ls
    where
        go _ [] = []
        go state (l:rest)
            | Text.stripEnd l == "processes:" = go (Just Nothing) rest
            | Nothing <- state = go Nothing rest
            | not (Text.null l), not (Text.isPrefixOf " " l), not (Text.isPrefixOf "\t" l) = []
            | Just mIndent <- state
            , Just (indent, name) <- parseIndentedKey l
            = case mIndent of
                Nothing -> name : go (Just (Just indent)) rest
                Just expected
                    | indent == expected -> name : go state rest
                    | indent < expected  -> []
                    | otherwise          -> go state rest
            | otherwise = go state rest

        parseIndentedKey line =
            let indent = Text.length (Text.takeWhile (== ' ') line)
                stripped = Text.strip line
            in if indent == 0 || Text.null stripped || Text.isPrefixOf "#" stripped
                then Nothing
                else case Text.breakOn ":" stripped of
                    (name, rest')
                        | not (Text.null rest'), not (Text.null name), Text.all isNameChar name
                        -> Just (indent, name)
                    _ -> Nothing

        isNameChar c = c == '_' || c == '-' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Filter log lines belonging to a specific service.
-- Matches the @"name  |"@ or @"name |"@ prefix that process-compose and overmind prepend.
filterServiceLines :: Text -> [Text] -> [Text]
filterServiceLines name = mapMaybe extractLine
    where
        extractLine line =
            case Text.breakOn "|" (Text.stripStart line) of
                (prefix, rest')
                    | not (Text.null rest'), Text.strip prefix == name
                    -> Just (Text.drop 1 rest')
                _ -> Nothing

-- | Filter out services that already have dedicated tabs.
filterBuiltinServices :: [Text] -> [Text]
filterBuiltinServices = filter (\name -> Text.toLower name /= "ihp" && Text.toLower name /= "postgres")

takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (max 0 (length xs - n)) xs
