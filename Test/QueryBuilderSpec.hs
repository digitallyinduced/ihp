{-|
Module: Test.QueryBuilderSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.QueryBuilderSpec where

import Test.Hspec
import IHP.Prelude
import IHP.QueryBuilder
import IHP.ModelSupport
import qualified Database.PostgreSQL.Simple.ToField as ToField
import Database.PostgreSQL.Simple.ToField (Action (..))
import qualified Data.ByteString.Builder as ByteString

data Post = Post
        { id :: UUID
        , title :: Text
        , externalUrl :: Maybe Text
        , createdAt :: UTCTime
        , public :: Bool
        , createdBy :: UUID
        }
type instance GetTableName Post = "posts"
type instance GetModelByTableName "posts" = Post

tests = do
    describe "QueryBuilder" do
        describe "query" do
            it "should provide a simple sql query" do
                let theQuery = query @Post

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts", [])

        describe "filterWhere" do
            it "should produce a SQL with a WHERE condition" do
                let theQuery = query @Post
                        |> filterWhere (#title, "Test" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE title = ?", [Escape "Test"])

            it "should use a IS operator for checking null" do
                let theQuery = query @Post
                        |> filterWhere (#externalUrl, Nothing)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE external_url IS ?", [Plain "null"])

        describe "filterWhereNot" do
            it "should produce a SQL with a WHERE NOT condition" do
                let theQuery = query @Post
                        |> filterWhereNot (#title, "Test" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE title != ?", [Escape "Test"])

            it "should use a IS NOT operator for checking null" do
                let theQuery = query @Post
                        |> filterWhereNot (#externalUrl, Nothing)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE external_url IS NOT ?", [Plain "null"])

        describe "filterWhereIn" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereIn (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE title IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])

        describe "filterWhereNotIn" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereNotIn (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE title NOT IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])

            it "ignore an empty value list as this causes the query to always return nothing" do
                let theValues :: [Text] = []
                let theQuery = query @Post
                        |> filterWhereNotIn (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts", [])
        
        describe "filterWhereSql" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereSql (#createdAt, "< current_timestamp - interval '1 day'")

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE created_at  ?", [Plain "< current_timestamp - interval '1 day'"])

        describe "filterWhereCaseInsensitive" do
            it "should produce a SQL with a WHERE LOWER() condition" do
                let theQuery = query @Post
                        |> filterWhereCaseInsensitive (#title, "Test" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE LOWER(title) = LOWER(?)", [Escape "Test"])


        describe "orderBy" do
            describe "orderByAsc" do
                it "should add a ORDER BY" do
                    let theQuery = query @Post
                            |> orderByAsc #createdAt

                    (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts ORDER BY created_at", [])
                
                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> orderByAsc #createdAt
                            |> orderByAsc #title

                    (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts ORDER BY created_at,title", [])
            
            describe "orderByDesc" do
                it "should add a ORDER BY DESC" do
                    let theQuery = query @Post
                            |> orderByDesc #createdAt

                    (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts ORDER BY created_at DESC", [])
                
                it "should accumulate multiple ORDER BY's" do
                    let theQuery = query @Post
                            |> orderByDesc #createdAt
                            |> orderByDesc #title

                    (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts ORDER BY created_at DESC,title DESC", [])
        
        describe "limit" do
            it "should add a LIMIT" do
                let theQuery = query @Post
                        |> limit 1337

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts LIMIT 1337", [])
        
        describe "offset" do
            it "should add a OFFSET" do
                let theQuery = query @Post
                        |> offset 1337

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts OFFSET 1337", [])
        
        describe "queryOr" do
            it "should merge two conditions" do
                let theQuery = query @Post
                        |> queryOr
                            (filterWhere (#createdBy, "fe41a985-36a3-4f14-b13c-c166977dc7e8" :: UUID))
                            (filterWhere (#public, True))


                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE (created_by = ?) OR (public = ?)", [Plain "'fe41a985-36a3-4f14-b13c-c166977dc7e8'", Plain "true"])

        describe "distinct" do
            it "should add a DISTINCT" do
                let theQuery = query @Post
                        |> distinct

                (toSQL theQuery) `shouldBe` ("SELECT DISTINCT posts.* FROM posts", [])
        
        describe "distinctOn" do
            it "should add a DISTINCT ON (..)" do
                let theQuery = query @Post
                        |> distinctOn #title

                (toSQL theQuery) `shouldBe` ("SELECT DISTINCT ON (title) posts.* FROM posts", [])

        describe "Complex Queries" do
            it "should allow a query with limit and offset" do
                let theQuery = query @Post
                        |> offset 20
                        |> limit 50

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts LIMIT 50 OFFSET 20", [])
            
            it "should work with multiple complex conditions" do
                let theQuery = query @Post
                        |> queryOr
                            (\qb -> qb
                                |> filterWhere (#title, "test")
                                |> filterWhere (#public, True)
                                |> filterWhere (#externalUrl, Nothing)
                            )
                            (filterWhere (#createdBy, "e70c66fb-68a5-41b8-8bf1-85b9bb046d15" :: UUID))
                        |> orderBy #createdAt
                        |> orderBy #title
                        |> limit 10

                (toSQL theQuery) `shouldBe` (
                        "SELECT posts.* FROM posts WHERE (((title = ?) AND (public = ?)) AND (external_url IS ?)) OR (created_by = ?) ORDER BY created_at,title LIMIT 10",
                        [ Escape "test"
                        , Plain "true"
                        , Plain "null"
                        , Plain "'e70c66fb-68a5-41b8-8bf1-85b9bb046d15'"
                        ]
                    )
