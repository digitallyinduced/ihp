module IHP.IDE.SchemaDesigner.Controller.Helper where

import IHP.ControllerPrelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import qualified Text.Megaparsec as Megaparsec
import qualified IHP.IDE.SchemaDesigner.Compiler as SchemaCompiler
import IHP.IDE.SchemaDesigner.View.Schema.Error

instance ParamReader PostgresType where
    readParameter byteString = case Megaparsec.runParser Parser.sqlType "" (cs byteString) of
        Left error -> Left (cs $ tshow error)
        Right result -> Right result

readSchema ::
    ( ?context::ControllerContext
    , ?modelContext::ModelContext
    , ?theAction::controller
    ) => IO [Statement]
readSchema = Parser.parseSchemaSql >>= \case
        Left error -> do render ErrorView { error }; pure []
        Right statements -> pure statements

getSqlError :: IO (Maybe ByteString)
getSqlError = Parser.parseSchemaSql >>= \case
        Left error -> do pure (Just error)
        Right statements -> do pure Nothing

updateSchema ::
    ( ?context :: ControllerContext
    , ?modelContext::ModelContext
    , ?theAction::controller
    ) => ([Statement] -> [Statement]) -> IO ()
updateSchema updateFn = do
    statements <- readSchema
    let statements' = updateFn statements
    SchemaCompiler.writeSchema statements'

getAllObjectNames :: [Statement] -> [Text]
getAllObjectNames = mapMaybe extractObjectName
    where
        extractObjectName (StatementCreateTable CreateTable { name }) = Just name
        extractObjectName CreateEnumType { name } = Just name
        extractObjectName _                       = Nothing

