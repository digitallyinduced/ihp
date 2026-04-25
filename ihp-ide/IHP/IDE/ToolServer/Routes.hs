{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module IHP.IDE.ToolServer.Routes where
import IHP.RouterPrelude
import IHP.Router.DSL (routes)
import IHP.IDE.ToolServer.Types

[routes|SchemaController
ANY        /PushToDb                                  PushToDbAction
ANY        /DumpDb                                    DumpDbAction
POST|PATCH /UpdateDb                                  UpdateDbAction
GET        /ShowCode                                  ShowCodeAction
ANY        /SaveCode                                  SaveCodeAction
GET        /ShowGeneratedCode?statementName           ShowGeneratedCodeAction
|]

[routes|TablesController
ANY        /Tables                                    TablesAction
GET        /ShowTable?tableName                       ShowTableAction
ANY        /NewTable                                  NewTableAction
POST       /CreateTable                               CreateTableAction
ANY        /EditTable?tableName&tableId               EditTableAction
POST|PATCH /UpdateTable                               UpdateTableAction
DELETE     /DeleteTable?tableId&tableName             DeleteTableAction
|]

[routes|ColumnsController
ANY        /NewColumn?tableName                                                      NewColumnAction
POST       /CreateColumn                                                             CreateColumnAction
ANY        /EditColumn?tableName&columnId                                            EditColumnAction
POST|PATCH /UpdateColumn                                                             UpdateColumnAction
DELETE     /DeleteColumn?tableName&columnId&columnName                               DeleteColumnAction
ANY        /ToggleColumnUnique?tableName&columnId                                    ToggleColumnUniqueAction
ANY        /NewForeignKey?tableName&columnName                                       NewForeignKeyAction
POST       /CreateForeignKey                                                         CreateForeignKeyAction
ANY        /EditForeignKey?tableName&columnName&constraintName&referenceTable        EditForeignKeyAction
POST|PATCH /UpdateForeignKey                                                         UpdateForeignKeyAction
DELETE     /DeleteForeignKey?constraintName&tableName                                DeleteForeignKeyAction
|]

[routes|PoliciesController
ANY        /NewPolicy?tableName                       NewPolicyAction
POST       /CreatePolicy                              CreatePolicyAction
ANY        /EditPolicy?tableName&policyName           EditPolicyAction
POST|PATCH /UpdatePolicy                              UpdatePolicyAction
DELETE     /DeletePolicy?tableName&policyName         DeletePolicyAction
|]

[routes|EnumsController
GET        /ShowEnum?enumName                         ShowEnumAction
ANY        /NewEnum                                   NewEnumAction
POST       /CreateEnum                                CreateEnumAction
ANY        /EditEnum?enumName&enumId                  EditEnumAction
POST|PATCH /UpdateEnum                                UpdateEnumAction
DELETE     /DeleteEnum?tableId                        DeleteEnumAction
|]

[routes|EnumValuesController
ANY        /NewEnumValue?enumName                     NewEnumValueAction
POST       /CreateEnumValue                           CreateEnumValueAction
ANY        /EditEnumValue?enumName&valueId            EditEnumValueAction
POST|PATCH /UpdateEnumValue                           UpdateEnumValueAction
DELETE     /DeleteEnumValue?enumName&valueId          DeleteEnumValueAction
|]

[routes|DataController
GET        /ShowDatabase                                                             ShowDatabaseAction
GET        /ShowTableRows?tableName                                                  ShowTableRowsAction
DELETE     /DeleteTableRows?tableName                                                DeleteTableRowsAction
ANY        /NewQuery                                                                 NewQueryAction
ANY        /Query                                                                    QueryAction
DELETE     /DeleteEntry?primaryKey&tableName                                         DeleteEntryAction
POST       /CreateRow                                                                CreateRowAction
ANY        /NewRow?tableName                                                         NewRowAction
ANY        /EditRow?tableName&targetPrimaryKey                                       EditRowAction
POST|PATCH /UpdateRow                                                                UpdateRowAction
ANY        /EditRowValue?tableName&targetName&id                                     EditRowValueAction
ANY        /ToggleBooleanField?tableName&targetName&targetPrimaryKey                 ToggleBooleanFieldAction
POST|PATCH /UpdateValue                                                              UpdateValueAction
GET        /ShowForeignKeyHoverCard?tableName&id&columnName                          ShowForeignKeyHoverCardAction
ANY        /AutocompleteForeignKeyColumn?tableName&columnName&term                   AutocompleteForeignKeyColumnAction
|]

[routes|LogsController
ANY        /AppLogs                                   AppLogsAction
ANY        /PostgresLogs                              PostgresLogsAction
ANY        /ServiceLogs?serviceName                   ServiceLogsAction
ANY        /OpenEditor                                OpenEditorAction
|]

[routes|CodeGenController
ANY        /Generators                                GeneratorsAction
ANY        /NewController                             NewControllerAction
ANY        /NewScript                                 NewScriptAction
ANY        /NewView                                   NewViewAction
ANY        /NewMail                                   NewMailAction
ANY        /NewAction                                 NewActionAction
ANY        /NewApplication                            NewApplicationAction
ANY        /NewJob                                    NewJobAction
POST       /CreateController                          CreateControllerAction
POST       /CreateScript                              CreateScriptAction
POST       /CreateView                                CreateViewAction
POST       /CreateMail                                CreateMailAction
POST       /CreateAction                              CreateActionAction
POST       /CreateApplication                         CreateApplicationAction
POST       /CreateJob                                 CreateJobAction
ANY        /OpenController                            OpenControllerAction
|]

[routes|MigrationsController
ANY        /Migrations                                MigrationsAction
ANY        /NewMigration                              NewMigrationAction
POST       /CreateMigration                           CreateMigrationAction
ANY        /EditMigration?migrationId                 EditMigrationAction
POST|PATCH /UpdateMigration?migrationId               UpdateMigrationAction
DELETE     /DeleteMigration?migrationId               DeleteMigrationAction
ANY        /RunMigration?migrationId                  RunMigrationAction
|]

[routes|IndexesController
ANY        /EditIndex?tableName&indexName             EditIndexAction
POST|PATCH /UpdateIndex?tableName&indexName           UpdateIndexAction
DELETE     /DeleteIndex?tableName&indexName           DeleteIndexAction
POST       /CreateIndex?tableName&columnName          CreateIndexAction
|]
