module Test.TypedSqlSpec where

import Web.Controller.Prelude hiding (get, request)
import IHP.FrameworkConfig
import IHP.Environment
import IHP.Test.Mocking
import IHP.Hspec (withIHPApp)
import IHP.TypedSql (typedSql, sqlQueryTyped, sqlExecTyped)
import Test.Hspec

import Web.FrontController ()

testConfig :: ConfigBuilder
testConfig = do
    option Development
    option (AppPort 8002)

-- | These specs exist primarily to guard the typedSql-safety of the nix
-- test checks (see flake-module.nix). Each [typedSql| … |] quasiquote
-- describes its SQL against a live PostgreSQL schema at COMPILE time. That
-- only works because the test check sets IHP_TYPED_SQL_AUTO_DB=1 (plus
-- IHP_LIB and postgres on PATH), which makes the quasiquoter boot a
-- throwaway database and load IHPSchema.sql + Application/Schema.sql before
-- describing. Remove that wiring and this module fails to compile.
tests :: Spec
tests = around (withIHPApp WebApplication testConfig) do
    describe "typedSql" do
        it "describes a SELECT at compile time and runs it at runtime" $ withContext do
            user <- newRecord @User
                |> set #email "typed@example.com"
                |> set #passwordHash "hash"
                |> createRecord

            newRecord @Post
                |> set #title "Typed Post"
                |> set #body "Body"
                |> set #userId user.id
                |> createRecord

            -- Result type ([Text]) is inferred by describing the query against
            -- the schema at compile time — no manual decoder.
            titles <- sqlQueryTyped [typedSql|
                SELECT title FROM posts ORDER BY title
            |]

            titles `shouldBe` ["Typed Post"]

        it "describes a parameterised UPDATE/SELECT" $ withContext do
            user <- newRecord @User
                |> set #email "param@example.com"
                |> set #passwordHash "hash"
                |> createRecord

            post <- newRecord @Post
                |> set #title "Before"
                |> set #body "Body"
                |> set #userId user.id
                |> createRecord

            -- Splices must be simple identifiers/parenthesised expressions: the
            -- quasiquoter parses `post.id` as `post . id`, so bind it first.
            let thePostId = post.id

            -- ${…} splices are typed too: a wrong-typed parameter would fail
            -- the compile-time describe.
            rowsAffected <- sqlExecTyped [typedSql|
                UPDATE posts SET title = ${("After" :: Text)} WHERE id = ${thePostId}
            |]
            rowsAffected `shouldBe` 1

            -- Cardinality is inferred: filtering on the primary key yields
            -- at most one row, so the result is @Maybe Text@ rather than a list.
            title <- sqlQueryTyped [typedSql|
                SELECT title FROM posts WHERE id = ${thePostId}
            |]
            title `shouldBe` Just "After"
