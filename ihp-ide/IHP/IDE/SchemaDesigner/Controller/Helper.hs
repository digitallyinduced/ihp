module IHP.IDE.SchemaDesigner.Controller.Helper where

import IHP.IDE.Prelude
import IHP.Postgres.Types
import qualified IHP.Postgres.Parser as Parser
import qualified IHP.IDE.SchemaDesigner.Parser as SchemaDesignerParser
import qualified Text.Megaparsec as Megaparsec
import qualified IHP.IDE.SchemaDesigner.Compiler as SchemaCompiler
import IHP.IDE.SchemaDesigner.View.Schema.Error
import IHP.IDE.ToolServer.Helper.Controller
import IHP.RequestBodyMiddleware (Respond)

instance ParamReader PostgresType where
    readParameter byteString = case Megaparsec.runParser Parser.sqlType "" (cs byteString) of
        Left error -> Left (cs $ tshow error)
        Right result -> Right result

instance ParamReader Expression where
    readParameter byteString = case Megaparsec.runParser Parser.expression "" (cs byteString) of
        Left parserError -> Left (cs $ Megaparsec.errorBundlePretty parserError)
        Right result -> Right result

instance ParamReader [IndexColumn] where
    readParameter byteString = case Megaparsec.runParser Parser.parseIndexColumns "" (cs byteString) of
        Left parserError -> Left (cs $ Megaparsec.errorBundlePretty parserError)
        Right result -> Right result

readSchema ::
    ( ?context::ControllerContext
    , ?modelContext::ModelContext
    , ?theAction::controller
    , ?respond::Respond
    ) => IO [Statement]
readSchema = SchemaDesignerParser.parseSchemaSql >>= \case
        Left error -> do render ErrorView { error }; pure []
        Right statements -> pure statements

getSqlError :: IO (Maybe ByteString)
getSqlError = SchemaDesignerParser.parseSchemaSql >>= \case
        Left error -> do pure (Just error)
        Right statements -> do pure Nothing

updateSchema ::
    ( ?context :: ControllerContext
    , ?modelContext::ModelContext
    , ?theAction::controller
    , ?respond::Respond
    ) => ([Statement] -> [Statement]) -> IO ()
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    SchemaCompiler.writeSchema statements'
    markDatabaseNeedsMigration

getAllObjectNames :: [Statement] -> [Text]
getAllObjectNames = mapMaybe extractObjectName
    where
        extractObjectName (StatementCreateTable CreateTable { name }) = Just name
        extractObjectName CreateEnumType { name } = Just name
        extractObjectName _                       = Nothing

