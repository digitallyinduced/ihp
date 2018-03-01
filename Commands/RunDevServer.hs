{-# LANGUAGE NamedFieldPuns #-}

module Main where
    import ClassyPrelude
    import qualified System.Process as Process
    import Twitch
    import System.Exit
    import System.Posix.Signals
    import qualified Control.Exception as Exception
    import qualified GHC.IO.Handle as Handle
    import System.Process.Internals

    runServerHs = "src/Foundation/Commands/RunServer.hs"

    data DevServerState = DevServerState {
            postgresProcess :: IORef (Handle, Process.ProcessHandle),
            serverProcess :: IORef (Handle, Process.ProcessHandle),
            compilerProcess :: IORef (Handle, Process.ProcessHandle)
        }

    initDevServerState = do
        postgresProcess <- startPostgres >>= newIORef
        serverProcess <- startPlainGhci >>= initServer >>= newIORef
        compilerProcess <- startPlainGhci >>= newIORef
        return $ DevServerState { postgresProcess = postgresProcess, serverProcess = serverProcess, compilerProcess = compilerProcess }

    registerExitHandler handler = do
        threadId <- myThreadId
        installHandler keyboardSignal (Catch (do { handler; Exception.throwTo threadId ExitSuccess })) Nothing


    main = do
        state <- initDevServerState
        registerExitHandler (cleanup state)
        watch state
        cleanup state

    cleanup :: DevServerState -> IO ()
    cleanup state = do
        let DevServerState { serverProcess, postgresProcess, compilerProcess } = state
        let processes = [serverProcess, postgresProcess, compilerProcess]
        let stopProcess process = do (_, p') <- readIORef serverProcess; Process.terminateProcess p'
        stopServer
        forM_ processes stopProcess

    startPlainGhci = do
        let process = (Process.proc "ghci" ["-threaded", "-isrc", "-isrc/Controller", "-isrc/Model", "-isrc/Generated", "-XOverloadedStrings", "-XNoImplicitPrelude", "-XImplicitParams", "-XRank2Types", "-XDisambiguateRecordFields", "-XNamedFieldPuns", "-XDuplicateRecordFields", "-fprint-potential-instances", "-XFlexibleContexts"]) { Process.std_in = Process.CreatePipe }
        (Just input, _, _, handle) <- Process.createProcess process
        return (input, handle)

    initServer ghci = do
        sendGhciCommand ghci ":l src/Foundation/SchemaCompiler.hs"
        sendGhciCommand ghci "c"
        sendGhciCommand ghci (":l " <> runServerHs)
        sendGhciCommand ghci ":l src/Foundation/UrlGeneratorCompiler.hs"
        sendGhciCommand ghci "c"
        sendGhciCommand ghci (":l " <> runServerHs)
        sendGhciCommand ghci "main"
        return ghci

    watch state@(DevServerState {serverProcess}) = defaultMain $ do
        "Controller/*.hs" |> const (rebuild serverProcess)
        "View/*/*.hs" |> const (rebuild serverProcess)
        "View/Context.hs" |> const (rebuild serverProcess)
        "Model/Schema.hs" |> const (rebuildModels state)
        "Model/*.hs" |> const (rebuild serverProcess)
        "Config.hs" |> const (rebuild serverProcess)
        "Foundation/*.hs" |> const (rebuild serverProcess)
        "Routes.hs" |> const (rebuildUrlGenerator state)
        "UrlGenerator.hs" |> const (rebuild serverProcess)


    waitASec = threadDelay $ 1 * 1000000

    rebuildModels (DevServerState {compilerProcess}) = do
        putStrLn "rebuildModels"
        ghci@(input, process) <- readIORef compilerProcess
        sendGhciCommand ghci ":l src/Foundation/SchemaCompiler.hs"
        sendGhciCommand ghci "c"
        putStrLn "rebuildModels => Finished"

    rebuildUrlGenerator (DevServerState {compilerProcess}) = do
        putStrLn "rebuildUrlGenerator"
        ghci@(input, process) <- readIORef compilerProcess
        sendGhciCommand ghci ":l src/Foundation/UrlGeneratorCompiler.hs"
        sendGhciCommand ghci "c"
        putStrLn "rebuildUrlGenerator => Finished"



    sendGhciInterrupt ghci@(input, process) = do
        pid <- getPid process
        case pid of
            Just pid -> signalProcess sigINT pid
            Nothing -> putStrLn "sendGhciInterrupt: failed, pid not found"

    rebuild serverProcess = do
        putStrLn "Rebuilding server"
        ghci <- readIORef serverProcess
        sendGhciInterrupt ghci
        sendGhciInterrupt ghci
        sendGhciCommand ghci ""
        sendGhciCommand ghci ":r"
        sendGhciCommand ghci "main"
        waitASec
        putStrLn "Restarted server"

    sendGhciCommand ghciProcess command = do
        let (input, process) = ghciProcess
        Handle.hPutStr input (command <> "\n")
        Handle.hFlush input

    stopServer = do
        putStrLn "stopServer called"
        _ <- Process.system "(lsof -i tcp:8000 | grep ghc | awk 'NR!=1 {print $2}' | xargs kill) || true"
        return ()

    stopPostgres = do
        _ <- Process.system "(lsof -i tcp:8001 | grep postgres | awk 'NR!=1 {print $2}' | xargs kill) || true"
        return ()

    startPostgres = do
        let process = (Process.proc "postgres" ["-D", "db/state", "-p", "8001"]) { Process.std_in = Process.CreatePipe }
        (Just input, _, _, handle) <- Process.createProcess process

        return (input, handle) 
        
    getPid ph = withProcessHandle ph go
        where
            go ph_ = case ph_ of
                OpenHandle x   -> return $ Just x
                ClosedHandle _ -> return Nothing
