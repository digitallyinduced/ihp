module Test.ScriptSupportSpec where

import IHP.Prelude
import IHP.ScriptSupport
import Test.Hspec
import qualified Control.Exception as Exception
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar
import qualified Control.Monad.IO.Class as MonadIO
import qualified GHC.IO.Encoding as Encoding
import qualified GHC.IO.Handle.Internals as Handle
import qualified System.IO as IO
import qualified System.Timeout as Timeout

data InitializationReached = InitializationReached deriving (Show, Eq)
instance Exception.Exception InitializationReached

-- Inspect initialization before opening a database connection.
checkConfiguration :: IO () -> ConfigBuilder
checkConfiguration check = MonadIO.liftIO do
    check
    Exception.throwIO InitializationReached

tests :: Spec
tests = describe "script encoding initialization" do
    it "preserves existing encodings in runScript" $ withAsciiEncodings do
        runScript (checkConfiguration (expectEncodings "ASCII")) (pure ())
            `shouldThrow` (== InitializationReached)

    it "initializes while another thread holds the stdin handle lock" do
        locked <- newEmptyMVar
        release <- newEmptyMVar
        let holdInput = Handle.withHandle_ "ScriptSupportSpec" IO.stdin \handle -> do
                putMVar locked ()
                takeMVar release
                pure handle
        Async.withAsync holdInput \_ -> do
            takeMVar locked
            let initialize = runScript (checkConfiguration (pure ())) (pure ())
                    `shouldThrow` (== InitializationReached)
            Timeout.timeout 1000000 initialize `shouldReturn` Just ()

    it "sets the UTF-8 locale in runScriptUtf8 and restores encodings on exceptions" $ withAsciiEncodings do
        let expectUtf8Locale = fmap show Encoding.getLocaleEncoding `shouldReturn` ("UTF-8" :: Text)
        runScriptUtf8 (checkConfiguration expectUtf8Locale) (pure ())
            `shouldThrow` (== InitializationReached)
        expectEncodings "ASCII"

expectEncodings :: Text -> IO ()
expectEncodings expected = do
    fmap show Encoding.getLocaleEncoding `shouldReturn` expected
    forEach [IO.stdin, IO.stdout, IO.stderr] \handle ->
        fmap (fmap show) (IO.hGetEncoding handle) `shouldReturn` Just expected

withAsciiEncodings :: IO a -> IO a
withAsciiEncodings action = Exception.bracket
    ((,) <$> Encoding.getLocaleEncoding <*> mapM IO.hGetEncoding handles)
    (\(locale, encodings) -> do
        Encoding.setLocaleEncoding locale
        forEach (zip handles encodings) \(handle, encoding) -> case encoding of
            Nothing -> IO.hSetBinaryMode handle True
            Just encoding -> IO.hSetEncoding handle encoding)
    (\_ -> do
        ascii <- IO.mkTextEncoding "ASCII"
        Encoding.setLocaleEncoding ascii
        forEach handles \handle -> IO.hSetEncoding handle ascii
        action)
    where
        handles = [IO.stdin, IO.stdout, IO.stderr]
