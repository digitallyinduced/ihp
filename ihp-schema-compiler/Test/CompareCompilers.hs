module Main where

import Prelude
import Control.Monad (forM, forM_, unless, when)
import qualified Data.Map.Strict as Map
import Data.List (sort)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Main.Utf8 (withUtf8)
import SchemaCompiler.TestSupport
import System.Directory (doesDirectoryExist, getModificationTime, makeAbsolute, removePathForcibly)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import System.IO.Temp (withSystemTempDirectory)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = withUtf8 do
    hSetBuffering stdout LineBuffering
    arguments <- getArgs
    case arguments of
        [baseline, optimized] -> run baseline optimized Nothing 1
        [baseline, optimized, schema] -> run baseline optimized (Just schema) 5
        [baseline, optimized, schema, count] -> case readMaybe count of
            Just runs | runs > 0 -> run baseline optimized (Just schema) runs
            _ -> die "RUNS must be a positive integer"
        _ -> die "Usage: compare-schema-compilers BASELINE_BINARY OPTIMIZED_BINARY [SCHEMA_SQL [RUNS]]"

run :: FilePath -> FilePath -> Maybe FilePath -> Int -> IO ()
run baseline optimized schema runs = do
    baselinePath <- makeAbsolute baseline
    optimizedPath <- makeAbsolute optimized
    forM_ [False, True] \relations -> do
        compareCompilers baselinePath optimizedPath fixtureSchema "fixture" relations 1
        forM_ schema \path -> do
            sql <- Text.readFile path
            compareCompilers baselinePath optimizedPath sql "schema" relations runs

compareCompilers :: FilePath -> FilePath -> Text.Text -> String -> Bool -> Int -> IO ()
compareCompilers baseline optimized sql name relations runs =
    withSystemTempDirectory "ihp-schema-benchmark-" \root -> do
        let reference = root </> "baseline"
        let actual = root </> "optimized"
        prepareSchema reference sql
        prepareSchema actual sql
        cold <- forM [1 .. runs] \_ -> do
            -- Remove previous output outside the timed operation. Each sample
            -- includes parsing, rendering, writing and initial cache creation.
            forM_ [reference, actual] \directory -> do
                exists <- doesDirectoryExist (directory </> "build")
                when exists (removePathForcibly (directory </> "build"))
            before <- runCompiler baseline reference relations
            after <- runCompiler optimized actual relations
            expected <- outputHashes reference
            generated <- outputHashes actual
            requireEqualOutputs expected generated
            pure (before, after)
        expected <- outputHashes actual
        times <- outputTimes actual expected
        cacheTime <- getModificationTime (cacheFile actual)
        warm <- forM [1 .. runs] \_ -> do
            before <- runCompiler baseline reference relations
            after <- runCompiler optimized actual relations
            pure (before, after)
        generated <- outputHashes actual
        requireEqualOutputs expected generated
        unchangedTimes <- outputTimes actual expected
        unchangedCacheTime <- getModificationTime (cacheFile actual)
        unless (times == unchangedTimes) (fail "Unchanged generated files were rewritten")
        unless (cacheTime == unchangedCacheTime) (fail "Unchanged run missed the cache")
        let before = median (map fst cold)
        let after = median (map snd cold)
        let warmBefore = median (map fst warm)
        let warmAfter = median (map snd warm)
        printf "%s, relations=%d: %d identical files; fresh %.3fs -> %.3fs (%.2fx); unchanged %.3fs -> %.3fs (%.2fx)\n"
            name (if relations then 1 else 0 :: Int) (Map.size generated)
            before after (before / after) warmBefore warmAfter (warmBefore / warmAfter)

median :: [Double] -> Double
median values = case splitAt (length values `div` 2) (sort values) of
    (lower, middle : _)
        | odd (length values) -> middle
        | previous : _ <- reverse lower -> (previous + middle) / 2
    _ -> error "median requires a non-empty sample"
