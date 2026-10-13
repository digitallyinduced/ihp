{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, runScriptUtf8, runDevScript, Script, module IHP.FrameworkConfig) where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.ModelSupport (withModelContext)
import Main.Utf8 (withUtf8)

-- | A script is just an IO action which requires a database connection and framework config
type Script = (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()

-- | Initializes IHP and runs the script without changing process encodings.
--
-- Use this inside an existing process, including asynchronous GHCi workers.
-- For a standalone executable's @main@, use 'runScriptUtf8'.
runScript :: ConfigBuilder -> Script -> IO ()
runScript configBuilder taskMain = do
    withFrameworkConfig configBuilder \frameworkConfig -> do
        withModelContext frameworkConfig.databaseUrl frameworkConfig.logger \modelContext -> do
            let ?modelContext = modelContext
            let ?context = frameworkConfig
            taskMain
{-# INLINABLE runScript #-}

-- | Runs a standalone script with a UTF-8 locale and transliteration enabled
-- for standard handles, using 'withUtf8'.
--
-- Call this at the executable entry point, before other threads use the standard
-- handles. Encoding initialization takes the stdin handle lock, so this must not
-- run in an asynchronous GHCi worker while GHCi is reading commands.
--
-- > main = runScriptUtf8 Config.config run
runScriptUtf8 :: ConfigBuilder -> Script -> IO ()
runScriptUtf8 configBuilder taskMain = withUtf8 (runScript configBuilder taskMain)
{-# INLINABLE runScriptUtf8 #-}

-- | Run a script interactively from GHCi using the default IHP config.
--
-- This reads DATABASE_URL from the environment (set by devenv) and uses
-- default settings for everything else. For custom config, use 'runScript' instead.
--
-- __Example:__ Run a script module from GHCi:
--
-- > import IHP.ScriptSupport
-- > :l Application/Script/HelloWorld.hs
-- > runDevScript run
--
-- __Example:__ Run inline script code:
--
-- > import IHP.ScriptSupport
-- > runDevScript do { users <- query @User |> fetch; forEach users \user -> putStrLn user.name }
--
runDevScript :: Script -> IO ()
runDevScript = runScript (pure ())