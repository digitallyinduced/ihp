module IHP.IDE.SchemaDesigner.Controller.Helper where

import IHP.ControllerPrelude
import IHP.IDE.SchemaDesigner.Types
import qualified IHP.IDE.SchemaDesigner.Parser as Parser
import qualified Text.Megaparsec as Megaparsec

instance ParamReader PostgresType where
    readParameter byteString = case Megaparsec.runParser Parser.sqlType "" (cs byteString) of
        Left error -> Left (cs $ tshow error)
        Right result -> Right result