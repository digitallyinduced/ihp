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
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpEqual NullExpression
                        , orderByClause = []
                        , limit = Nothing
                        , offset = Nothing
                        }
                
                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) IS NULL"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id"]
                        )

            it "compile 'field <> NULL' conditions to 'field IS NOT NULL'" do
                let query = DynamicSQLQuery
                        { table = "posts"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "userId") OpNotEqual NullExpression
                        , orderByClause = []
                        , limit = Nothing
                        , offset = Nothing
                        }
                
                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) IS NOT NULL"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id"]
                        )

            it "compile queries with TS expressions" do
                let query = DynamicSQLQuery
                        { table = "products"
                        , selectedColumns = SelectAll
                        , whereCondition = Just $ InfixOperatorExpression (ColumnExpression "ts") OpTSMatch (CallExpression { functionCall = ToTSQuery { text = "test" }})
                        , orderByClause = [ OrderByTSRank { tsvector = "ts", tsquery = "test" } ]
                        , limit = Nothing
                        , offset = Nothing
                        }
                
                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) @@ (to_tsquery('english', ?)) ORDER BY ts_rank(?, to_tsquery('english', ?))"
                        , [PG.Plain "*", PG.EscapeIdentifier "products", PG.EscapeIdentifier "ts", PG.Escape "test", PG.EscapeIdentifier "ts", PG.Escape "test"]
                        )