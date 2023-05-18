{-|
Module: Test.IDE.CodeGeneration.MailGenerator/ID
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
import IHP.IDE.Defaults.TableColumnDefaults

tests = do
    describe "Mail Generator Tests:" do
        let schema = [
                    StatementCreateTable $ defCreateTablePKID "users" ["id"] [idColumn]
                    ]
        it "should build a mail with name \"PurchaseConfirmationMail\"" do
            let mailName = "PurchaseConfirmationMail"
            let rawControllerName = "Users"
            let controllerName = tableNameToControllerName rawControllerName
            let modelName = tableNameToModelName rawControllerName
            let applicationName = "Web"
            let config = MailGenerator.MailConfig { .. }
            let builtPlan = MailGenerator.buildPlan' schema config

            builtPlan `shouldBe` [
                  EnsureDirectory {directory = "Web/Mail/Users"}
                , CreateFile {filePath = "Web/Mail/Users/PurchaseConfirmation.hs", fileContent = "module Web.Mail.Users.PurchaseConfirmation where\nimport Web.View.Prelude\nimport IHP.MailPrelude\n\ndata PurchaseConfirmationMail = PurchaseConfirmationMail { user :: User }\n\ninstance BuildMail PurchaseConfirmationMail where\n    subject = \"Subject\"\n    to PurchaseConfirmationMail { .. } = Address { addressName = Just \"Firstname Lastname\", addressEmail = \"fname.lname@example.com\" }\n    from = \"hi@example.com\"\n    html PurchaseConfirmationMail { .. } = [hsx|\n        Hello World\n    |]\n"}
                , AddImport {filePath = "Web/Controller/Users.hs", fileContent = "import Web.Mail.Users.PurchaseConfirmation"}
                ]
