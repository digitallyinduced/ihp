module Main where
    import ClassyPrelude
    import qualified System.Process as Process
    import Twitch
    import System.Exit
    import System.Posix.Signals
    import qualified Control.Exception as Exception
    import qualified GHC.IO.Handle as Handle

    runServerHs = "src/Foundation/Commands/RunServer.hs"

    main = do
        postgresProcess <- startPostgres >>= newIORef
        serverProcess <- startGhci >>= newIORef
        threadId <- myThreadId
        installHandler keyboardSignal (Catch (do { cleanup serverProcess postgresProcess; Exception.throwTo threadId ExitSuccess })) Nothing
        watch serverProcess
        cleanup serverProcess postgresProcess

    cleanup serverProcess postgresProcess = do
        stopServer
        (_, p') <- readIORef serverProcess
        Process.terminateProcess p'
        (_, p') <- readIORef postgresProcess
        Process.terminateProcess p'

    startGhci = do
        let process = (Process.proc "ghci" ["-threaded", "-isrc", "-isrc/Controller", "-isrc/Model", "-isrc/Generated", "-XOverloadedStrings", "-XNoImplicitPrelude", "-XImplicitParams", "-XRank2Types"]) { Process.std_in = Process.CreatePipe }
        (Just input, _, _, handle) <- Process.createProcess process
        let ghci = (input, handle)
        threadDelay $ 1 * 1000000
        putStrLn "Loading Modules"
        sendGhciCommand ghci (":l " <> runServerHs)
        threadDelay $ 1 * 1000000
        sendGhciCommand ghci "main"
        threadDelay $ 1 * 1000000
        return (input, handle) 
    watch serverProcess = defaultMain $ do
        "**/*.hs" |> const (rebuild serverProcess)
        "Foundation/*.hs" |> const (rebuild serverProcess)

    rebuild serverProcess = do
        putStrLn "Rebuilding"
        ghci@(input, process) <- readIORef serverProcess
        stopServer
        sendGhciCommand ghci ":r"
        sendGhciCommand ghci "main"
        putStrLn "Restarted server"

    sendGhciCommand ghciProcess command = do
        let (input, process) = ghciProcess
        Handle.hPutStr input (command <> "\n")
        Handle.hFlush input

    stopServer = do
        _ <- Process.system "(lsof -i tcp:8000 | grep ghc | awk 'NR!=1 {print $2}' | xargs kill) || true"
        return ()

    stopPostgres = do
        _ <- Process.system "(lsof -i tcp:8001 | grep postgres | awk 'NR!=1 {print $2}' | xargs kill) || true"
        return ()

    rebuildModels serverProcess = do
        ghci@(input, process) <- readIORef serverProcess
        stopServer
        --sendGhciCommand ghci ":r"
        --sendGhciCommand ghci "rebuildModels"
        sendGhciCommand ghci ":r"
        sendGhciCommand ghci "main"

    startPostgres = do
        let process = (Process.proc "postgres" ["-D", "db/state", "-p", "8001"]) { Process.std_in = Process.CreatePipe }
        (Just input, _, _, handle) <- Process.createProcess process

        return (input, handle) 
        
