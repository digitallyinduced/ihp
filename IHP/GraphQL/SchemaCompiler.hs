module IHP.GraphQL.SchemaCompiler where

import IHP.Prelude
import IHP.GraphQL.Types

import IHP.IDE.SchemaDesigner.Types


type SqlSchema = [Statement]
type GraphQLSchema = [Definition]

sqlSchemaToGraphQLSchema :: SqlSchema -> GraphQLSchema
sqlSchemaToGraphQLSchema statements =
            [ schemaDefinition
            , queryDefinition statements
            , mutationDefinition statements
            ]
            <> customScalars
            <> recordTypes statements
            <> newRecordTypes statements
            <> patchTypes statements

schemaDefinition :: Definition
schemaDefinition =
    TypeSystemDefinition { typeSystemDefinition = SchemaDefinition
        { queryType = NamedType "Query"
        , mutationType = NamedType "Mutation"
        }
    }

customScalars :: [Definition]
customScalars =
    ["UUID", "Timestamp"]
    |> map (TypeSystemDefinition . TypeDefinition . ScalarTypeDefinition)

queryDefinition :: SqlSchema -> Definition
queryDefinition statements = TypeSystemDefinition { typeSystemDefinition = TypeDefinition typeDefinition }
    where
        typeDefinition =
            ObjectTypeDefinition
                { name = "Query"
                , implementsInterfaces = []
                , fieldDefinitions = mconcat $ map statementToQueryField statements
                }


mutationDefinition :: SqlSchema -> Definition
mutationDefinition statements = TypeSystemDefinition { typeSystemDefinition = TypeDefinition typeDefinition }
    where
        typeDefinition =
            ObjectTypeDefinition
                { name = "Mutation"
                , implementsInterfaces = []
                , fieldDefinitions = mconcat $ map statementToMutationFields statements
                }

statementToQueryField :: Statement -> [FieldDefinition]
statementToQueryField (StatementCreateTable CreateTable { name }) = 
        [ manyRecordsField ]
    where
        manyRecordsField = FieldDefinition
            { description = Just ("Returns all records from the `" <> name <> "` table")
            , name = lcfirst (tableNameToControllerName name)
            , argumentsDefinition = []
            , type_
            }
        type_ = NonNullType (ListType (NonNullType (NamedType (tableNameToModelName name))))
statementToQueryField _ = []

statementToMutationFields :: Statement -> [FieldDefinition]
statementToMutationFields (StatementCreateTable CreateTable { name }) = 
        [ createRecord, updateRecord, deleteRecord ]
    where
        createRecord = FieldDefinition
            { description = Nothing
            , name = "create" <> tableNameToModelName name
            , argumentsDefinition =
                [ ArgumentDefinition { name = lcfirst (tableNameToModelName name), argumentType = NonNullType (NamedType ("New" <> tableNameToModelName name)), defaultValue = Nothing }
                ]
            , type_ = NonNullType (NamedType (tableNameToModelName name))
            }
        updateRecord = FieldDefinition
            { description = Nothing
            , name = "update" <> tableNameToModelName name
            , argumentsDefinition =
                [ ArgumentDefinition { name = "id", argumentType = NonNullType (NamedType "ID"), defaultValue = Nothing }
                , ArgumentDefinition { name = "patch", argumentType = NonNullType (NamedType ((tableNameToModelName name) <> "Patch")), defaultValue = Nothing }
                ]
            , type_ = NonNullType (NamedType (tableNameToModelName name))
            }
        deleteRecord = FieldDefinition
            { description = Nothing
            , name = "delete" <> tableNameToModelName name
            , argumentsDefinition =
                [ ArgumentDefinition { name = "id", argumentType = NonNullType (NamedType "ID"), defaultValue = Nothing }
                ]
            , type_ = NonNullType (NamedType (tableNameToModelName name))
            }

statementToMutationFields _ = []

recordTypes :: [Statement] -> [Definition]
recordTypes statements = mapMaybe (recordType statements) statements

recordType :: SqlSchema -> Statement -> Maybe Definition
recordType schema (StatementCreateTable table@(CreateTable { name, columns })) = 
        Just TypeSystemDefinition { typeSystemDefinition = TypeDefinition typeDefinition }
    where
        typeDefinition =
            ObjectTypeDefinition
                { name = tableNameToModelName name
                , implementsInterfaces = []
                , fieldDefinitions = (map (columnToRecordField table) columns) <> foreignKeyFields
                }
        foreignKeyFields =
            schema
            |> mapMaybe foreignKeyToHasManyField

        foreignKeyToHasManyField (AddConstraint { tableName = fkTable, constraint = ForeignKeyConstraint { columnName = localColumn, referenceTable }}) | referenceTable == name = 
            Just FieldDefinition
                { description = Nothing
                , name = lcfirst (tableNameToControllerName fkTable)
                , argumentsDefinition = []
                , type_ = NonNullType (ListType (NonNullType (NamedType (tableNameToModelName fkTable))))
                }
        foreignKeyToHasManyField _ = Nothing
recordType _ _ = Nothing

newRecordTypes :: [Statement] -> [Definition]
newRecordTypes statements = mapMaybe (newRecordType statements) statements

newRecordType :: SqlSchema -> Statement -> Maybe Definition
newRecordType schema (StatementCreateTable table@(CreateTable { name, columns })) = 
        Just TypeSystemDefinition { typeSystemDefinition = TypeDefinition typeDefinition }
    where
        typeDefinition =
            InputObjectTypeDefinition
                { name = "New" <> tableNameToModelName name
                , fieldDefinitions = map (columnToRecordField table) columns
                }
newRecordType _ _ = Nothing

patchTypes :: [Statement] -> [Definition]
patchTypes statements = mapMaybe (patchType statements) statements

patchType :: SqlSchema -> Statement -> Maybe Definition
patchType schema (StatementCreateTable table@(CreateTable { name, columns })) = 
        Just TypeSystemDefinition { typeSystemDefinition = TypeDefinition typeDefinition }
    where
        typeDefinition =
            InputObjectTypeDefinition
                { name = tableNameToModelName name <> "Patch"
                , fieldDefinitions = map (columnToRecordField table) columns
                }
patchType _ _ = Nothing

columnToRecordField :: CreateTable -> Column -> FieldDefinition
columnToRecordField table Column { name, columnType, notNull } = 
        FieldDefinition
            { description = Nothing
            , name = columnNameToFieldName name
            , argumentsDefinition = []
            , type_ = 
                if isPrimaryKey
                    then NonNullType (NamedType "ID")
                    else (if notNull
                        then NonNullType
                        else \v -> v) $ postgresTypeToGraphQLType columnType
            }
    where
        primaryKeyColumns = table
                |> get #primaryKeyConstraint
                |> get #primaryKeyColumnNames
        isPrimaryKey = name `elem` primaryKeyColumns

postgresTypeToGraphQLType :: PostgresType -> Type
postgresTypeToGraphQLType PText = NamedType "String"
postgresTypeToGraphQLType PUUID = NamedType "UUID"
postgresTypeToGraphQLType PInt = NamedType "Int"
postgresTypeToGraphQLType PSmallInt = NamedType "Int"
postgresTypeToGraphQLType PBigInt = NamedType "BigInt"
postgresTypeToGraphQLType PBoolean = NamedType "Boolean"
postgresTypeToGraphQLType PTimestamp = NamedType "Timestamp"
postgresTypeToGraphQLType PTimestampWithTimezone = NamedType "Timestamp"
postgresTypeToGraphQLType PReal = NamedType "Float"
postgresTypeToGraphQLType PDouble = NamedType "Float"
postgresTypeToGraphQLType PPoint = NamedType "Point"
postgresTypeToGraphQLType PPolygon = error "todo"
postgresTypeToGraphQLType PDate = NamedType "Date"
postgresTypeToGraphQLType PBinary = NamedType "String"
postgresTypeToGraphQLType PTime = NamedType "Time"
postgresTypeToGraphQLType (PNumeric _ _) = NamedType "Float"
postgresTypeToGraphQLType (PVaryingN _) = NamedType "String"
postgresTypeToGraphQLType (PCharacterN _) = NamedType "String"
postgresTypeToGraphQLType PSingleChar = NamedType "String"
postgresTypeToGraphQLType PSerial = NamedType "Int"
postgresTypeToGraphQLType PBigserial = NamedType "BigInt"
postgresTypeToGraphQLType PJSONB = NamedType "JSON"
postgresTypeToGraphQLType PInet = NamedType "IPv4"
postgresTypeToGraphQLType PTSVector = NamedType "String"
postgresTypeToGraphQLType (PArray type_) = ListType (postgresTypeToGraphQLType type_)
postgresTypeToGraphQLType PTrigger = error "Trigger cannot be converted to a GraphQL type"
postgresTypeToGraphQLType (PCustomType theType) = NamedType "String"