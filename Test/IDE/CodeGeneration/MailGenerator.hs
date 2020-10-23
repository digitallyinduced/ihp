{-|
Module: Test.IDE.CodeGeneration.MailGenerator
Copyright: (c) digitally induced GmbH, 2020
-}
module Test.IDE.CodeGeneration.MailGenerator where

import Test.Hspec
import IHP.Prelude
import qualified IHP.IDE.CodeGen.MailGenerator as MailGenerator
import IHP.ViewPrelude (cs, plain)
import qualified Text.Megaparsec as Megaparsec
import IHP.IDE.CodeGen.Types
import IHP.IDE.SchemaDesigner.Types
import IHP.NameSupport


tests = do
    describe "Mail Generator Tests:" do
        let schema = [
                    StatementCreateTable CreateTable {
                        name = "users"
                        , columns = [
                            Column
                                { name = "id"
                                , columnType = PUUID
                                , defaultValue = Just (CallExpression "uuid_generate_v4" [])
                                , notNull = True
                                , isUnique = False
                                }
                        ]
                        , primaryKeyConstraint = PrimaryKeyConstraint ["id"]
                        , constraints = []
                        }
                    ]
        it "should build a mail with name \"ConfirmationMail\"" do
            let mailName = "ConfirmationMail"
            let rawControllerName = "Users"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let config = MailGenerator.MailConfig { .. }
            let builtPlan = MailGenerator.buildPlan' schema config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Mail/Users"}
                , CreateFile {filePath = "Web/Mail/Users/Confirmation.hs", fileContent = "module Web.Mail.Users.Confirmation where\nimport Web.View.Prelude\nimport IHP.MailPrelude\n\ndata ConfirmationMail = ConfirmationMail { user :: User }\n\ninstance BuildMail ConfirmationMail where\n    subject = \"Subject\"\n    to ConfirmationMail { .. } = Address { addressName = Just \"Firstname Lastname\", addressEmail = \"fname.lname@example.com\" }\n    from = \"hi@example.com\"\n    html ConfirmationMail { .. } = [hsx|\n        Hello World\n    |]\n"}
                , AddImport {filePath = "Web/Controller/Users.hs", fileContent = "import Web.Mail.Users.Confirmation"}
                ]
