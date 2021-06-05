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

data User = User
    { id :: UUID,
      name :: Text
    }

type instance GetTableName User = "users"
type instance GetModelByTableName "users" = User 

data FavoriteTitle = FavoriteTitle
    {
        title :: Text,
        likes :: Int
    }

type instance GetTableName FavoriteTitle = "favorite_title"
type instance GetModelByTableName "favorite_title" = FavoriteTitle 

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

        describe "filterWhereInJoinedTable" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @User
                        |> innerJoin @Post (#name, #title)
                        |> filterWhereInJoinedTable @Post (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT users.* FROM users INNER JOIN posts ON users.name = posts.title WHERE posts.title IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])


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

        describe "filterWhereNotInJoinedTable" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @User
                        |> innerJoin @Post (#name, #title)
                        |> filterWhereNotInJoinedTable @Post (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT users.* FROM users INNER JOIN posts ON users.name = posts.title WHERE posts.title NOT IN ?", [Many [Plain "(", Escape "first", Plain ",", Escape "second", Plain ")"]])

            it "ignore an empty value list as this causes the query to always return nothing" do
                let theValues :: [Text] = []
                let theQuery = query @User
                        |> innerJoin @Post (#name, #title)
                        |> filterWhereNotInJoinedTable @Post (#title, theValues)

                (toSQL theQuery) `shouldBe` ("SELECT users.* FROM users INNER JOIN posts ON users.name = posts.title", [])

        describe "filterWhereILike" do
            it "should produce a SQL with a WHERE condition" do
                let searchTerm = "good"
                let theQuery = query @Post
                     |> filterWhereILike (#title, "%" <> searchTerm <> "%")
                (toSQL theQuery `shouldBe` ("SELECT posts.* FROM posts WHERE title ILIKE ?", [Escape "%good%"]))

        describe "filterWhereILikeJoinedTable" do
            it "should produce a SQL with a WHERE condition" do
                let searchTerm = "louis"
                let theQuery = query @Post
                     |> innerJoin @User (#createdBy, #id)
                     |> filterWhereILikeJoinedTable @User (#name, "%" <> searchTerm <> "%")
                (toSQL theQuery `shouldBe` ("SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id WHERE users.name ILIKE ?", [Escape "%louis%"]))
        
        describe "filterWhereSql" do
            it "should produce a SQL with a WHERE condition" do
                let theValues :: [Text] = ["first", "second"]
                let theQuery = query @Post
                        |> filterWhereSql (#createdAt, "< current_timestamp - interval '1 day'")

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts WHERE created_at  ?", [Plain "< current_timestamp - interval '1 day'"])

        describe "innerJoin" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title", [])


        describe "innerJoinThirdTable" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)
                        |> innerJoinThirdTable @User @FavoriteTitle (#name, #title)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title INNER JOIN users ON favorite_title.title = users.name", [])

        describe "filterWhereJoinedTable" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)
                        |> filterWhereJoinedTable @User (#name, "Tom" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title WHERE users.name = ?", [Escape "Tom"])

        describe "filterWhereNotJoinedTable" do
            it "should provide an inner join sql query" do
                let theQuery = query @Post
                        |> innerJoin @User (#createdBy, #id)
                        |> innerJoin @FavoriteTitle (#title, #title)
                        |> filterWhereNotJoinedTable @User (#name, "Tom" :: Text)

                (toSQL theQuery) `shouldBe` ("SELECT posts.* FROM posts INNER JOIN users ON posts.created_by = users.id INNER JOIN favorite_title ON posts.title = favorite_title.title WHERE users.name != ?", [Escape "Tom"])



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
