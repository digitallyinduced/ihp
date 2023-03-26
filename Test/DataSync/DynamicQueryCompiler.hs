{-|
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.DataSync.DynamicQueryCompiler where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.DynamicQuery
import IHP.QueryBuilder hiding (OrderByClause)
import qualified Database.PostgreSQL.Simple.ToField as PG

tests = do
    describe "IHP.DataSync.DynamicQueryCompiler" do
        describe "compileQuery" do
            it "compile a basic select query" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts"]
                        )

            it "compile a select query with order by" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , distinctOnColumn = Nothing
                        , orderByClause = [OrderByClause { orderByColumn = "title", orderByDirection = Desc }]
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? ORDER BY ? ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "title", PG.Plain "DESC"]
                        )

            it "compile a select query with multiple order bys" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , orderByClause = [
                            OrderByClause { orderByColumn = "createdAt", orderByDirection = Desc },
                            OrderByClause { orderByColumn = "title", orderByDirection = Asc }
                        ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? ORDER BY ? ?, ? ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "created_at", PG.Plain "DESC", PG.EscapeIdentifier "title", PG.Plain ""]
                        )

            it "compile a basic select query with a where condition" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) = (?)"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Escape "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"]
                        )

            it "compile a basic select query with a where condition and an order by" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = [ OrderByClause { orderByColumn = "createdAt", orderByDirection = Desc } ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) = (?) ORDER BY ? ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Escape "b8553ce9-6a42-4a68-b5fc-259be3e2acdc", PG.EscapeIdentifier "created_at", PG.Plain "DESC"]
                        )

            it "compile a basic select query with a limit" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , distinctOnColumn = Nothing
                        , orderByClause = []
                        , limit = Just 50
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) = (?) LIMIT ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Escape "b8553ce9-6a42-4a68-b5fc-259be3e2acdc", PG.Plain "50"]
                        )

            it "compile a basic select query with an offset" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Just 50
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) = (?) OFFSET ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Escape "b8553ce9-6a42-4a68-b5fc-259be3e2acdc", PG.Plain "50"]
                        )

            it "compile a basic select query with a limit and an offset" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression (TextValue "b8553ce9-6a42-4a68-b5fc-259be3e2acdc"))
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Just 25
                        , offset = Just 50
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) = (?) LIMIT ? OFFSET ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Escape "b8553ce9-6a42-4a68-b5fc-259be3e2acdc", PG.Plain "25", PG.Plain "50"]
                        )

            it "compile 'field = NULL' conditions to 'field IS NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual (LiteralExpression Null)
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) IS ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Plain "null"]
                        )

            it "compile 'field <> NULL' conditions to 'field IS NOT NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpNotEqual (LiteralExpression Null)
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) IS NOT ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Plain "null"]
                        )
            
            it "compile 'field IN (NULL)' conditions to 'field IS NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "a") OpIn (ListExpression { values = [Null] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) IS ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "a", PG.Plain "null"]
                        )
            it "compile 'field IN (NULL, 'string')' conditions to 'field IS NULL OR field IN ('string')'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "a") OpIn (ListExpression { values = [Null, TextValue "test" ] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE ((?) IN ?) OR ((?) IS ?)"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "a", PG.Many [PG.Plain "(", PG.Escape "test", PG.Plain ")"], PG.EscapeIdentifier "a", PG.Plain "null"]
                        )

            it "compile queries with TS expressions" do
                let query = DynamicSQLQuery
                        { table = "products"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "ts") OpTSMatch (CallExpression { functionCall = ToTSQuery { text = "test" }})
                        , orderByClause = [ OrderByTSRank { tsvector = "ts", tsquery = "test" } ]
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) @@ (to_tsquery('english', ?)) ORDER BY ts_rank(?, to_tsquery('english', ?))"
                        , [PG.Plain "*", PG.EscapeIdentifier "products", PG.EscapeIdentifier "ts", PG.Escape "test", PG.EscapeIdentifier "ts", PG.Escape "test"]
                        )

            it "compile a basic select query with distinctOn" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Nothing
                        , orderByClause = []
                        , distinctOnColumn = Just "groupId"
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT DISTINCT ON (?) ? FROM ?"
                        , [PG.EscapeIdentifier "group_id", PG.Plain "*", PG.EscapeIdentifier "posts"]
                        )

            it "compile a WHERE IN query" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "id") OpIn (ListExpression { values = [UUIDValue "a5d7772f-c63f-4444-be69-dd9afd902e9b", UUIDValue "bb88d55a-1ed0-44ad-be13-d768f4b3f9ca"] })
                        , orderByClause = []
                        , distinctOnColumn = Nothing
                        , limit = Nothing
                        , offset = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) IN ?"
                        ,
                            [ PG.Plain "*"
                            , PG.EscapeIdentifier "posts"
                            , PG.EscapeIdentifier "id"
                            , PG.Many
                                [ PG.Plain "("
                                , PG.Plain "'a5d7772f-c63f-4444-be69-dd9afd902e9b'"
                                , PG.Plain ","
                                , PG.Plain "'bb88d55a-1ed0-44ad-be13-d768f4b3f9ca'"
                                , PG.Plain ")"
                                ]
                            ]
                        )
