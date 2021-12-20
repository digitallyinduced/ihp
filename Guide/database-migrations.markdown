# Database Migrations

```toc

```

## Introduction

For development purposes, you already know the `Update DB` workflow from the Schema Designer. Once your app is running in production you will at some point need to make changes to your database schema. With migrations, you can evolve your production database schema by writing short SQL statements to make the necessary changes.

In IHP a migration file is a plain SQL file consisting of `ALTER TABLE` and other SQL statements that patch the database. They are stored in `Application/Migration` and are named by a timestamp and an optional description.

Here's how a directory structure with a few migrations can look like:

```bash
Application/Migration/
    1604852690-create-users-table.sql
    1604852210-add-confirmed_at-to-users.sql
    1604847925-create-projects-table.sql
```

## Generating a Migration

Open the [Code Generator](http://localhost:8001/Generators) and click `Migration`. You need to enter an optional description, e.g. `Create Posts Table`. The description will be sluggified to `create-posts-table` when generating the migration file.

Click `Generate`. Your editor will open with the new `.sql` file.

You can also call this from the command line:

```bash
new-migration "Create Posts Table"
```

To generate our new posts table we copy the DDL statement from `Schema.sql` into this migration:

```sql
CREATE TABLE posts (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL
);
```

After that, we can already run this migration to update our production database.

## Running Migrations

IHP provides a shell command `migrate` that runs all migrations that haven't be executed yet. A table `schema_migratons` is used to keep track of which migrations already have been run. The table will be automatically created by the `migrate` tool.

To test your migrations locally, you can run this tool locally like:

```bash
migrate
```

In a production context you need to specify the correct database URL via the `DATABASE_URL` environment variable:

```
DATABASE_URL=postgresql://... migrate
```

### Running Migrations on IHP Cloud

If you use IHP Cloud the migrations will be automatically executed during the deployment. Nothing to do for you :-)


### Skipping Old Migrations

You can set the `MINIMUM_REVISION` env variable when running `migrate` to ignore migration revisions older than the specified unix timestamp:

```bash
export MINIMUM_REVISION=1000

# 'Application/Migration/999-old-migration.sql' would be ignored now when running 'migrate'
migrate
```

A good value for `MINIMUM_REVISION` is typically the unix timestamp of the time when the database was initially created.

## Common Issues

### ALTER TYPE ... ADD cannot run inside a transaction block

When running a miration like this:

```sql
ALTER TYPE my_enum ADD VALUE 'some_value';
```

you will typical get an error like this:

```
Query (89.238182ms): "BEGIN" ()
migrate: SqlError {sqlState = "25001", sqlExecStatus = FatalError, sqlErrorMsg = "ALTER TYPE ... ADD cannot run inside a transaction block", sqlErrorDetail = "", sqlErrorHint = ""}
```

IHP implicit wraps the migration within a transaction. You can disable this implicit transaction manually like this:

```sql
COMMIT; -- Commit the transaction previously started by IHP
ALTER TYPE my_enum ADD VALUE 'some_value';
BEGIN; -- Restart the connection as IHP will also try to run it's own COMMIT
```
