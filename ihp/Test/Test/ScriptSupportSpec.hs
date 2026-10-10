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

-- Stop at configuration, before database initialization, to test the real
-- runScript entry point without requiring a database.
checkInitialization :: IO ()
checkInitialization =
    runScript (MonadIO.liftIO (Exception.throwIO InitializationReached)) (pure ())
        `shouldThrow` (== InitializationReached)

tests :: Spec
tests = describe "runScript encoding initialization" do
    it "sets existing output handles to UTF-8" $ withOutputEncodings do
        ascii <- IO.mkTextEncoding "ASCII"
        IO.hSetEncoding IO.stdout ascii
        IO.hSetEncoding IO.stderr ascii
        Encoding.setLocaleEncoding ascii
        checkInitialization
        fmap show (IO.hGetEncoding IO.stdout) `shouldReturn` "Just UTF-8"
        fmap show (IO.hGetEncoding IO.stderr) `shouldReturn` "Just UTF-8"
        fmap show Encoding.getLocaleEncoding `shouldReturn` "UTF-8"

    it "initializes while another thread holds the stdin handle lock" $ withOutputEncodings do
        locked <- newEmptyMVar
        release <- newEmptyMVar
        let holdInput = Handle.withHandle_ "ScriptSupportSpec" IO.stdin \handle -> do
                putMVar locked ()
                takeMVar release
                pure handle
        Async.withAsync holdInput \_ -> do
            takeMVar locked
            Timeout.timeout 1000000 checkInitialization `shouldReturn` Just ()

withOutputEncodings :: IO a -> IO a
withOutputEncodings action = Exception.bracket
    ((,,) <$> Encoding.getLocaleEncoding <*> IO.hGetEncoding IO.stdout <*> IO.hGetEncoding IO.stderr)
    (\(locale, output, errors) -> do
        Encoding.setLocaleEncoding locale
        restore IO.stdout output
        restore IO.stderr errors)
    (\_ -> action)
    where
        restore handle Nothing = IO.hSetBinaryMode handle True
        restore handle (Just encoding) = IO.hSetEncoding handle encoding
