{-|
Module: IHP.ScriptSupport
Description: Run scripts inside the framework context, but outside of the usual web request response lifecycle
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.ScriptSupport (runScript, Script, module IHP.FrameworkConfig) where

import IHP.Prelude
import IHP.FrameworkConfig
import IHP.ModelSupport
import Main.Utf8 (withUtf8)

-- | A script is just an IO action which requires a database connection and framework config
type Script = (?modelContext :: ModelContext, ?context :: FrameworkConfig) => IO ()

-- | Initializes IHP and then runs the script inside the framework context
runScript :: ConfigBuilder -> Script -> IO ()
runScript configBuilder taskMain = withUtf8 do
    withFrameworkConfig configBuilder \frameworkConfig -> do
        withModelContext frameworkConfig \modelContext -> do
            let ?modelContext = modelContext
            let ?context = frameworkConfig
            taskMain
{-# INLINABLE runScript #-}