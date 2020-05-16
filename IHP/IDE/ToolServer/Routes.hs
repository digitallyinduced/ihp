module IHP.IDE.ToolServer.Routes where
import IHP.RouterPrelude
import IHP.IDE.ToolServer.Types

instance AutoRoute SchemaController where
    parseArgument = parseTextArgument

instance AutoRoute TablesController where
    parseArgument = parseTextArgument

instance AutoRoute ColumnsController where
    parseArgument = parseTextArgument

instance AutoRoute EnumsController where
    parseArgument = parseTextArgument

instance AutoRoute EnumValuesController where
    parseArgument = parseTextArgument


instance AutoRoute LogsController
instance AutoRoute DataController