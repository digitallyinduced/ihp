module Test.IDE.SchemaDesigner.SchemaOperationsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

tests = do
    describe "IHP.IDE.SchemaDesigner.SchemaOperations" do
        let tableA = StatementCreateTable CreateTable { name = "a", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
        let tableB = StatementCreateTable CreateTable { name = "b", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
        describe "addEnum" do
            it "should prepend the enum" do
                let inputSchema = [tableA, tableB]
                let enum = CreateEnumType { name = "test", values = [] }
                let expectedSchema = [enum, tableA, tableB ]

                (SchemaOperations.addEnum "test" inputSchema) `shouldBe` expectedSchema
