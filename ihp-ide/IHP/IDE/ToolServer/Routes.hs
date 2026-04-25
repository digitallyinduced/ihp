{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module IHP.IDE.ToolServer.Routes where
import IHP.RouterPrelude
import IHP.Router.DSL (routes)
import IHP.IDE.ToolServer.Types

[routes|SchemaController
POST       /PushToDb                                  PushToDbAction
POST       /DumpDb                                    DumpDbAction
POST|PATCH /UpdateDb                                  UpdateDbAction
GET        /ShowCode                                  ShowCodeAction
POST       /SaveCode                                  SaveCodeAction
GET        /ShowGeneratedCode?statementName           ShowGeneratedCodeAction
|]

[routes|TablesController
GET        /Tables                                    TablesAction
GET        /ShowTable?tableName                       ShowTableAction
GET        /NewTable                                  NewTableAction
POST       /CreateTable                               CreateTableAction
GET        /EditTable?tableName&tableId               EditTableAction
POST|PATCH /UpdateTable                               UpdateTableAction
DELETE     /DeleteTable?tableId&tableName             DeleteTableAction
|]

[routes|ColumnsController
GET        /NewColumn?tableName                                                      NewColumnAction
POST       /CreateColumn                                                             CreateColumnAction
GET        /EditColumn?tableName&columnId                                            EditColumnAction
POST|PATCH /UpdateColumn                                                             UpdateColumnAction
DELETE     /DeleteColumn?tableName&columnId&columnName                               DeleteColumnAction
GET        /ToggleColumnUnique?tableName&columnId                                    ToggleColumnUniqueAction
GET        /NewForeignKey?tableName&columnName                                       NewForeignKeyAction
POST       /CreateForeignKey                                                         CreateForeignKeyAction
GET        /EditForeignKey?tableName&columnName&constraintName&referenceTable        EditForeignKeyAction
POST|PATCH /UpdateForeignKey                                                         UpdateForeignKeyAction
DELETE     /DeleteForeignKey?constraintName&tableName                                DeleteForeignKeyAction
|]

[routes|PoliciesController
GET        /NewPolicy?tableName                       NewPolicyAction
POST       /CreatePolicy                              CreatePolicyAction
GET        /EditPolicy?tableName&policyName           EditPolicyAction
POST|PATCH /UpdatePolicy                              UpdatePolicyAction
DELETE     /DeletePolicy?tableName&policyName         DeletePolicyAction
|]

[routes|EnumsController
GET        /ShowEnum?enumName                         ShowEnumAction
GET        /NewEnum                                   NewEnumAction
POST       /CreateEnum                                CreateEnumAction
GET        /EditEnum?enumName&enumId                  EditEnumAction
POST|PATCH /UpdateEnum                                UpdateEnumAction
DELETE     /DeleteEnum?tableId                        DeleteEnumAction
|]

[routes|EnumValuesController
GET        /NewEnumValue?enumName                     NewEnumValueAction
POST       /CreateEnumValue                           CreateEnumValueAction
GET        /EditEnumValue?enumName&valueId            EditEnumValueAction
POST|PATCH /UpdateEnumValue                           UpdateEnumValueAction
DELETE     /DeleteEnumValue?enumName&valueId          DeleteEnumValueAction
|]

[routes|DataController
GET        /ShowDatabase                                                             ShowDatabaseAction
GET        /ShowTableRows?tableName                                                  ShowTableRowsAction
DELETE     /DeleteTableRows?tableName                                                DeleteTableRowsAction
GET        /NewQuery                                                                 NewQueryAction
GET        /Query                                                                    QueryAction
DELETE     /DeleteEntry?primaryKey&tableName                                         DeleteEntryAction
POST       /CreateRow                                                                CreateRowAction
GET        /NewRow?tableName                                                         NewRowAction
GET        /EditRow?tableName&targetPrimaryKey                                       EditRowAction
POST|PATCH /UpdateRow                                                                UpdateRowAction
GET        /EditRowValue?tableName&targetName&id                                     EditRowValueAction
GET        /ToggleBooleanField?tableName&targetName&targetPrimaryKey                 ToggleBooleanFieldAction
POST|PATCH /UpdateValue                                                              UpdateValueAction
GET        /ShowForeignKeyHoverCard?tableName&id&columnName                          ShowForeignKeyHoverCardAction
GET        /AutocompleteForeignKeyColumn?tableName&columnName&term                   AutocompleteForeignKeyColumnAction
|]

[routes|LogsController
GET        /AppLogs                                   AppLogsAction
GET        /PostgresLogs                              PostgresLogsAction
GET        /ServiceLogs?serviceName                   ServiceLogsAction
GET        /OpenEditor                                OpenEditorAction
|]

[routes|CodeGenController
GET        /Generators                                GeneratorsAction
GET        /NewController                             NewControllerAction
GET        /NewScript                                 NewScriptAction
GET        /NewView                                   NewViewAction
GET        /NewMail                                   NewMailAction
GET        /NewAction                                 NewActionAction
GET        /NewApplication                            NewApplicationAction
GET        /NewJob                                    NewJobAction
POST       /CreateController                          CreateControllerAction
POST       /CreateScript                              CreateScriptAction
POST       /CreateView                                CreateViewAction
POST       /CreateMail                                CreateMailAction
POST       /CreateAction                              CreateActionAction
POST       /CreateApplication                         CreateApplicationAction
POST       /CreateJob                                 CreateJobAction
GET        /OpenController                            OpenControllerAction
|]

[routes|MigrationsController
GET        /Migrations                                MigrationsAction
GET        /NewMigration                              NewMigrationAction
POST       /CreateMigration                           CreateMigrationAction
GET        /EditMigration?migrationId                 EditMigrationAction
POST|PATCH /UpdateMigration?migrationId               UpdateMigrationAction
DELETE     /DeleteMigration?migrationId               DeleteMigrationAction
POST       /RunMigration?migrationId                  RunMigrationAction
|]

[routes|IndexesController
GET        /EditIndex?tableName&indexName             EditIndexAction
POST|PATCH /UpdateIndex?tableName&indexName           UpdateIndexAction
DELETE     /DeleteIndex?tableName&indexName           DeleteIndexAction
POST       /CreateIndex?tableName&columnName          CreateIndexAction
|]
