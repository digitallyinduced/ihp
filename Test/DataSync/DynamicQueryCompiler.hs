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
                        ( "SELECT ? FROM ??"
                        , [PG.Plain "*", PG.EscapeIdentifier "posts", PG.Plain ""]
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
