module Test.IDE.SchemaDesigner.SchemaOperationsSpec where

import Test.Hspec
import IHP.Prelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.SchemaOperations as SchemaOperations

tests = do
    describe "IHP.IDE.SchemaDesigner.SchemaOperations" do
        let tableA = StatementCreateTable CreateTable { name = "a", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
        let tableB = StatementCreateTable CreateTable { name = "b", columns = [], primaryKeyConstraint = PrimaryKeyConstraint [], constraints = [] }
        let enumA = CreateEnumType { name = "enumA", values = [] }
        let enumB = CreateEnumType { name = "enumB", values = [] }
        let comment = Comment { content = "comment" }
        describe "addEnum" do
            it "should prepend the enum" do
                let inputSchema = [tableA, tableB]
                let expectedSchema = [enumA, tableA, tableB ]

                (SchemaOperations.addEnum "enumA" inputSchema) `shouldBe` expectedSchema

            it "should prepend the enum, but after existing enums" do

                let inputSchema = [enumA, tableA]
                let expectedSchema = [enumA, enumB, tableA]

                (SchemaOperations.addEnum "enumB" inputSchema) `shouldBe` expectedSchema
            
            it "should deal with the empty case" do
                let inputSchema = []
                let expectedSchema = [enumA]

                (SchemaOperations.addEnum "enumA" inputSchema) `shouldBe` expectedSchema

            it "should ignore comments" do
                let inputSchema = [comment, enumA, tableA]
                let expectedSchema = [comment, enumA, enumB, tableA]

                (SchemaOperations.addEnum "enumB" inputSchema) `shouldBe` expectedSchema
