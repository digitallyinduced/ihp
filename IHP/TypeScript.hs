module IHP.TypeScript where

import Data.Aeson.TypeScript.Internal
import Data.Aeson.TypeScript.Recursive
import Data.Aeson.TypeScript.TH
import Data.Either
import IHP.IDE.SchemaDesigner.Parser
import IHP.IDE.SchemaDesigner.Types
import IHP.Job.Types
import IHP.Postgres.Point
import IHP.Postgres.Polygon
import IHP.Prelude
import qualified System.Directory as Directory
import System.IO (writeFile)
import qualified Text.Inflections as Inflector

generatedDeclarationsFilePath :: Text
generatedDeclarationsFilePath = "build/Generated/Types.d.ts"

formattingOptions :: FormattingOptions
formattingOptions = defaultFormattingOptions
    { numIndentSpaces = 4
    , exportMode = ExportEach
    }

generateTypeScriptDeclarations :: IO ()
generateTypeScriptDeclarations = generateTypeScriptDeclarations' generatedDeclarationsFilePath

generateTypeScriptDeclarations' :: Text -> IO ()
generateTypeScriptDeclarations' filePath = do
    statements <- parseSchemaFile
    let compiled = commonDeclarations <> (statements |> map compileStatement |> catMaybes)
    let formatted = formatTSDeclarations' formattingOptions compiled
    writeFile (cs filePath) formatted

parseSchemaFile :: IO [Statement]
parseSchemaFile =
    parseSchemaSql >>= \case
        Left parserError -> fail (cs parserError)
        Right statements -> pure statements

compileStatements :: [Statement] -> [TSDeclaration]
compileStatements statements = undefined

compileStatement :: Statement -> Maybe TSDeclaration
compileStatement (StatementCreateTable CreateTable { name, columns, primaryKeyConstraint, constraints }) =
    Just TSInterfaceDeclaration
        { interfaceName = cs $ tableNameToModelName name
        , interfaceMembers = columnsCompiled
        , interfaceGenericVariables = []
        }
    where
        columnsCompiled =
            columns
                |> map (\column ->
                    TSField { fieldName = cs $ textToCamelCase $ get #name column
                            , fieldType = cs $ postgresTypeToTypeScriptType (get #columnType column)
                            , fieldOptional = not $ get #notNull column
                            }
                    )
compileStatement CreateEnumType { name, values } =
    Just TSTypeAlternatives
        { typeName = cs $ textToUpperCamelCase name
        , alternativeTypes = enumValues
        , typeGenericVariables = []
        }
    where
        enumValues = values |> map (cs . textToUnderScore)
compileStatement _ = Nothing

textToCamelCase :: Text -> Text
textToCamelCase text = Inflector.toCamelCased False text |> fromRight text

textToUpperCamelCase :: Text -> Text
textToUpperCamelCase text = Inflector.toCamelCased True text |> fromRight text

textToUnderScore :: Text -> Text
textToUnderScore text = tshow $ Inflector.toUnderscore text |> fromRight text

postgresTypeToTypeScriptType :: PostgresType -> Text
postgresTypeToTypeScriptType PUUID                  = "UUID"
postgresTypeToTypeScriptType PTSVector              = "string"
postgresTypeToTypeScriptType PTimestampWithTimezone = "string"
postgresTypeToTypeScriptType PTimestamp             = "string"
postgresTypeToTypeScriptType PTime                  = "string"
postgresTypeToTypeScriptType PText                  = "string"
postgresTypeToTypeScriptType PSmallInt              = "number"
postgresTypeToTypeScriptType PSingleChar            = "string"
postgresTypeToTypeScriptType PSerial                = "number"
postgresTypeToTypeScriptType PReal                  = "number"
postgresTypeToTypeScriptType PPolygon               = "Polygon"
postgresTypeToTypeScriptType PPoint                 = "Point"
postgresTypeToTypeScriptType PJSONB                 = "string"
postgresTypeToTypeScriptType PInt                   = "number"
postgresTypeToTypeScriptType PInet                  = "string"
postgresTypeToTypeScriptType PDouble                = "number"
postgresTypeToTypeScriptType PDate                  = "string"
postgresTypeToTypeScriptType PBoolean               = "boolean"
postgresTypeToTypeScriptType PBinary                = "string"
postgresTypeToTypeScriptType PBigserial             = "number"
postgresTypeToTypeScriptType PBigInt                = "bigint"
postgresTypeToTypeScriptType (PVaryingN _)          = "string"
postgresTypeToTypeScriptType (PNumeric _ _)         = "number"
postgresTypeToTypeScriptType (PCharacterN _)        = "string"
postgresTypeToTypeScriptType (PCustomType typeName) = tableNameToModelName typeName
postgresTypeToTypeScriptType (PArray  arrayType)    = "Array<" <> postgresTypeToTypeScriptType arrayType <> ">"
postgresTypeToTypeScriptType PTrigger               = error "Trigger cannot be converted to a TypeScript type"


commonDeclarations :: [TSDeclaration]
commonDeclarations
    = getTypeScriptDeclarations (Proxy :: Proxy UUID)
    <> getTypeScriptDeclarations (Proxy :: Proxy Point)
    <> getTypeScriptDeclarations (Proxy :: Proxy Polygon)
    <> getTypeScriptDeclarations (Proxy :: Proxy JobStatus)

instance TypeScript UUID where
    getTypeScriptType _ = "UUID"
    getTypeScriptDeclarations _ =
        [ TSTypeAlternatives
            { typeName = "UUID"
            , typeGenericVariables = []
            , alternativeTypes = ["string"]
            }
        ]

instance TypeScript Point where
    getTypeScriptType _ = "Point"
    getTypeScriptDeclarations _ =
        [ TSInterfaceDeclaration
            { interfaceName = "Point"
            , interfaceMembers =
                [ TSField { fieldName = "x", fieldType = "number", fieldOptional = False }
                , TSField { fieldName = "y", fieldType = "number", fieldOptional = False }
                ]
            , interfaceGenericVariables = []
            }
        ]

instance TypeScript Polygon where
    getTypeScriptType _ = "Polygon"
    getTypeScriptDeclarations _ =
        [ TSInterfaceDeclaration
            { interfaceName = "Polygon"
            , interfaceMembers =
                [ TSField { fieldName = "points", fieldType = "Array<Point>", fieldOptional = False }
                ]
            , interfaceGenericVariables = []
            }
        ]

instance TypeScript JobStatus where
    getTypeScriptType _ = "JobStatus"
    getTypeScriptDeclarations _ =
        [ TSTypeAlternatives
            { typeName = "JobStatus"
            , typeGenericVariables = []
            , alternativeTypes = enumValues
            }
        ]
        where
            enumValues = allEnumValues @JobStatus |> map (cs. textToUnderScore . tshow)
