-- Raw statements exercise the selected driver independently of typedSql's
-- schema introspection. Writes use a connection-local temporary table.
module Test.HasqlAdapterSpec (tests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (cancel, waitCatch, withAsync)
import Control.Exception (bracket)
import qualified Data.ByteString as ByteString
import Data.Either (isLeft)
import Data.Int (Int32, Int64)
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Hasql.Connection as Connection
import qualified Hasql.Connection.Settings as Settings
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Errors as Errors
import qualified Hasql.Pool as Pool
import qualified Hasql.Pool.Config as PoolConfig
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction as Transaction
import qualified Hasql.Transaction.Sessions as Transactions
import qualified Pqi.Ffi
import System.Environment (getEnv, lookupEnv)
import System.Timeout (timeout)
import Test.Hspec
import Prelude

tests :: Spec
tests = describe "libpq Hasql adapter" do
    it "decodes a large result set without monopolizing the runtime" $
        withPool \pool -> do
            let count = 80000 :: Int32
                statement = Statement.unpreparable
                    "select i::int4, null::text from generate_series(1, $1::int4) as i"
                    (Encoders.param (Encoders.nonNullable Encoders.int4))
                    (Decoders.rowVector ((,)
                        <$> Decoders.column (Decoders.nonNullable Decoders.int4)
                        <*> Decoders.column (Decoders.nullable Decoders.text)))
            actual <- timeout 5000000 (Pool.use pool (Session.statement count statement))
            actual `shouldBe` Just (Right
                (Vector.generate (fromIntegral count) (\index -> (fromIntegral index + 1, Nothing))))

    it "preserves a binary field spanning many socket reads" $
        withPool \pool -> do
            let byteLength = 1024 * 1024
                statement = Statement.unpreparable
                    "select decode(repeat('ab', $1::int4), 'hex')"
                    (Encoders.param (Encoders.nonNullable Encoders.int4))
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bytea)))
            bytes <- expectRight =<< Pool.use pool (Session.statement (fromIntegral byteLength :: Int32) statement)
            ByteString.length bytes `shouldBe` byteLength
            ByteString.all (== 0xab) bytes `shouldBe` True

    it "rolls back a failed transaction and reuses the pool connection" $
        withPool \pool -> do
            expectRight =<< Pool.use pool (Session.script
                "create temporary table hasql_adapter_rollback_test (value int not null check (value > 0))")
            let insert = Statement.unpreparable
                    "insert into hasql_adapter_rollback_test values ($1)"
                    (Encoders.param (Encoders.nonNullable Encoders.int4)) Decoders.noResult
            result <- Pool.use pool $ Transactions.transactionNoRetry Transactions.ReadCommitted Transactions.Write do
                Transaction.statement (1 :: Int32) insert
                Transaction.statement (0 :: Int32) insert
            case result of
                Left (Pool.SessionUsageError failure) -> Errors.toSqlState failure `shouldBe` Just "23514"
                _ -> expectationFailure ("Expected a check violation, got " <> show result)
            count <- expectRight =<< Pool.use pool (Session.statement ()
                (Statement.unpreparable "select count(*) from hasql_adapter_rollback_test" Encoders.noParams
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8)))))
            count `shouldBe` (0 :: Int64)

    it "keeps the pool usable after cancelling an active query" $
        withPool \pool -> withConnection \control -> do
            processId <- expectRight =<< Pool.use pool (Session.statement ()
                (Statement.unpreparable "select pg_backend_pid()" Encoders.noParams
                    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))))
            withAsync (Pool.use pool (Session.script "select pg_sleep(1)")) \query -> do
                timeout 2_000_000 (awaitSleep control processId) `shouldReturn` Just ()
                cancel query
                waitCatch query >>= (`shouldSatisfy` isLeft)
            timeout 2_000_000 (Pool.use pool (Session.statement () selectOne)) `shouldReturn` Just (Right 1)

withPool :: (Pool.Pool -> IO ()) -> IO ()
withPool action = lookupEnv "DATABASE_URL" >>= \case
    Nothing -> pendingWith "Set DATABASE_URL to run the PostgreSQL adapter tests"
    Just databaseUrl -> bracket
        (Pool.acquire Pqi.Ffi.adapter (PoolConfig.settings
            [ PoolConfig.size 1, PoolConfig.acquisitionTimeout 2
            , PoolConfig.staticConnectionSettings (Settings.connectionString (Text.pack databaseUrl)) ]))
        Pool.release action

withConnection :: (Connection.Connection -> IO a) -> IO a
withConnection action = do
    settings <- Settings.connectionString . Text.pack <$> getEnv "DATABASE_URL"
    bracket (expectRight =<< Connection.acquire Pqi.Ffi.adapter settings) Connection.release action

selectOne :: Statement.Statement () Int32
selectOne = Statement.unpreparable "select 1::int4" Encoders.noParams
    (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int4)))

awaitSleep :: Connection.Connection -> Int32 -> IO ()
awaitSleep control processId = do
    sleeping <- expectRight =<< Connection.use control (Session.statement processId
        (Statement.unpreparable
            "select exists (select from pg_stat_activity where pid = $1 and wait_event = 'PgSleep')"
            (Encoders.param (Encoders.nonNullable Encoders.int4))
            (Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.bool)))))
    if sleeping then pure () else threadDelay 1000 >> awaitSleep control processId

expectRight :: Show error => Either error value -> IO value
expectRight = either (fail . show) pure
