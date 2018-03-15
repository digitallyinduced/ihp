{-# LANGUAGE NamedFieldPuns #-}

module Main where
    import ClassyPrelude hiding (threadDelay)
    import qualified System.Process as Process
    import Twitch
    import System.Exit
    import System.Posix.Signals
    import qualified Control.Exception as Exception
    import qualified GHC.IO.Handle as Handle
    import System.Process.Internals
    import Data.String.Conversions (cs)
    import Control.Concurrent (threadDelay)

    runServerHs = "src/Foundation/Commands/RunServer.hs"

    data DevServerState = DevServerState {
            postgresProcess :: IORef (Handle, Process.ProcessHandle),
            serverProcess :: IORef (Handle, Process.ProcessHandle),
            modelCompilerProcess :: IORef (Handle, Process.ProcessHandle),
            urlGeneratorCompilerProcess :: IORef (Handle, Process.ProcessHandle)
        }

    initDevServerState = do
        postgresProcess <- startPostgres >>= newIORef
        serverProcess <- startPlainGhci >>= initServer >>= newIORef
        modelCompilerProcess <- startCompileGhci >>= newIORef
        urlGeneratorCompilerProcess <- startCompileGhci >>= newIORef
        return $ DevServerState { postgresProcess = postgresProcess, serverProcess = serverProcess, modelCompilerProcess = modelCompilerProcess, urlGeneratorCompilerProcess = urlGeneratorCompilerProcess }

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
        putStrLn "cleanup"
        let DevServerState { serverProcess, postgresProcess, modelCompilerProcess, urlGeneratorCompilerProcess } = state
        let processes = [serverProcess, postgresProcess, modelCompilerProcess, urlGeneratorCompilerProcess]
        let stopProcess process = do (_, p') <- readIORef serverProcess; Process.terminateProcess p'
        stopServer
        forM_ processes stopProcess

    startPlainGhci = do
        let process = (Process.proc "ghci" ["-threaded", "-isrc", "-fprint-potential-instances"]) { Process.std_in = Process.CreatePipe }
        (Just input, _, _, handle) <- Process.createProcess process
        return (input, handle)

    startCompileGhci = do
        let process = (Process.proc "ghci" ["-threaded", "-isrc", "-w"]) { Process.std_in = Process.CreatePipe }
        (Just input, _, _, handle) <- Process.createProcess process
        return (input, handle)

    initServer ghci = do
        sendGhciCommand ghci (":script src/Foundation/startDevServerGhciScript")
        return ghci

    watch state@(DevServerState {serverProcess}) = defaultMain $ do
        "Controller/*.hs" |> const (rebuild serverProcess)
        "View/*/*.hs" |> const (rebuild serverProcess)
        "View/Context.hs" |> const (rebuild serverProcess)
        "Model/Schema.hs" |> const (rebuildModels state)
        "Model/*.hs" |> const (rebuild serverProcess)
        "Model/Generated/*.hs" |> const (rebuild serverProcess)
        "Config.hs" |> const (rebuild serverProcess)
        "Foundation/**.hs" |> const (rebuild serverProcess)
        "Routes.hs" |> const (rebuildUrlGenerator state)
        "UrlGenerator.hs" |> const (rebuild serverProcess)


    rebuildModels (DevServerState {modelCompilerProcess}) = do
        putStrLn "rebuildModels"
        ghci@(input, process) <- readIORef modelCompilerProcess
        sendGhciCommand ghci ":!clear"
        sendGhciCommand ghci ":l src/Foundation/SchemaCompiler.hs"
        sendGhciCommand ghci "c"
        putStrLn "rebuildModels => Finished"

    rebuildUrlGenerator (DevServerState {urlGeneratorCompilerProcess}) = do
        putStrLn "rebuildUrlGenerator"
        ghci@(input, process) <- readIORef urlGeneratorCompilerProcess
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
        sendGhciCommand ghci "run"
        putStrLn "Restarted server"

    sendGhciCommand ghciProcess command = do
        let (input, process) = ghciProcess
        putStrLn $ "Sending to ghci: " <> cs command
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
