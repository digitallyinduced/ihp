{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, runDevScript, Script, module IHP.FrameworkConfig) where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.ModelSupport (withModelContext)
import qualified GHC.IO.Encoding as IO
import qualified System.IO as IO

-- | A script is just an IO action which requires a database connection and framework config
type Script = (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()

-- | Initializes IHP and then runs the script inside the framework context.
-- Sets the locale, stdout and stderr encodings to UTF-8. The caller must
-- configure stdin before starting concurrent readers if UTF-8 input is needed.
runScript :: ConfigBuilder -> Script -> IO ()
runScript configBuilder taskMain = do
    -- 'Main.Utf8.withUtf8' reads the encoding of stdin, which needs the stdin handle lock.
    -- GHCi holds that lock while it waits for its next command, so a script started
    -- from a GHCi thread (e.g. the dev job worker) would wait forever.
    IO.setLocaleEncoding IO.utf8
    -- Standard handles already exist, so changing the locale does not update them.
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8

    withFrameworkConfig configBuilder \frameworkConfig -> do
        withModelContext frameworkConfig.databaseUrl frameworkConfig.logger \modelContext -> do
            let ?modelContext = modelContext
            let ?context = frameworkConfig
            taskMain
{-# INLINABLE runScript #-}

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