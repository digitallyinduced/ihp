{-|
Copyright: (c) digitally induced GmbH, 2021
-}
module Test.DataSync.DynamicQueryCompiler where

import Test.Hspec
import IHP.Prelude
import IHP.DataSync.DynamicQueryCompiler
import IHP.DataSync.DynamicQuery
import IHP.QueryBuilder
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
                        , limitClause = Nothing
                        , offsetClause = Nothing
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
                        , limitClause = Nothing
                        , offsetClause = Nothing
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
                        , limitClause = Nothing
                        , offsetClause = Nothing
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
                        , limitClause = Nothing
                        , offsetClause = Nothing
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
                        , limitClause = Nothing
                        , offsetClause = Nothing
                        }

                compileQuery query `shouldBe`
                        ( "SELECT ? FROM ? WHERE (?) = (?) ORDER BY ? ?"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.EscapeIdentifier "user_id", PG.Escape "b8553ce9-6a42-4a68-b5fc-259be3e2acdc", PG.EscapeIdentifier "created_at", PG.Plain "DESC"]
                        )