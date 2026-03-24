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

To test your migrations locally, first add `ihp-migrate` to your `flake.nix` if you haven't already:

```diff
                     haskellPackages = p: with p; [
                         # Haskell dependencies go here
                         p.ihp
+                        p.ihp-migrate
```
then you can run this tool locally like:

```bash
migrate
```

In a production context you need to specify the correct database URL via the `DATABASE_URL` environment variable:

```
DATABASE_URL=postgresql://... migrate
```

For running migrations when deploying from Github Actions, you can use `nix run .#migrate`.

### Skipping Old Migrations

You can set the `MINIMUM_REVISION` env variable when running `migrate` to ignore migration revisions older than the specified unix timestamp:

```bash
export MINIMUM_REVISION=1000

# 'Application/Migration/999-old-migration.sql' would be ignored now when running 'migrate'
migrate
```

A good value for `MINIMUM_REVISION` is typically the unix timestamp of the time when the database was initially created.


### IHP MIGRATIONS DIR

In production when running the migrations binary it is sometimes convenient to have all your Migrations in a non-standard place:
e.g. if you need to push migrations onto production server without rebuilding the application. There is an Environment variable
`IHP_MIGRATION_DIR` to accomplish this.

```
IHP_MIGRATION_DIR=path/to/my/migration/dir
```

This can be set in the environment attribute set of your IHP app flake.


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

Note that IHP's migration generator handles this automatically. When you use the Schema Designer to add a new enum value and then generate a migration, the generated SQL will already include the `COMMIT` / `BEGIN` workaround.

### Column Already Exists / Doesn't Exist

If you see an error like:

```
column "my_column" of relation "my_table" already exists
```

This means the migration is trying to add a column that is already present in the database. This typically happens when a migration was partially applied (e.g., you ran the SQL manually before running `migrate`), or when the same change was introduced in two different migrations.

To fix this, you can either:

1. Mark the migration as already applied by inserting its revision into `schema_migrations` manually:

    ```sql
    INSERT INTO schema_migrations (revision) VALUES (1604852690);
    ```

2. Or modify the migration to use `IF NOT EXISTS` (for `CREATE TABLE` or `ADD COLUMN`):

    ```sql
    ALTER TABLE posts ADD COLUMN IF NOT EXISTS slug TEXT;
    ```

The reverse problem -- "column does not exist" -- happens when a migration tries to alter or drop a column that is not in the database. Double-check the column name for typos, and verify that an earlier migration actually created it.

### Foreign Key Constraint Violations

When a migration adds a foreign key constraint to a table that already has data, you may see:

```
ERROR: insert or update on table "posts" violates foreign key constraint "posts_ref_user_id"
```

This happens when existing rows reference a value that does not exist in the referenced table. Before adding the constraint, clean up any orphaned rows:

```sql
-- Remove rows that reference a non-existent user
DELETE FROM posts WHERE user_id NOT IN (SELECT id FROM users);

-- Now it is safe to add the constraint
ALTER TABLE posts ADD CONSTRAINT posts_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE CASCADE;
```

### Migration Fails Partway Through

Each migration runs inside a database transaction by default. If any statement fails, the entire migration is rolled back and no partial changes are applied. This means you can safely fix the migration SQL and run `migrate` again.

The exception is migrations that contain the `COMMIT` / `BEGIN` workaround for enum values (see above). Those statements run outside the transaction, so if a later statement fails, the enum change will already be applied. In that case, you may need to adjust the migration SQL before re-running.

## Rolling Back Migrations

IHP migrations are forward-only. There is no built-in `rollback` command. Each migration is a plain SQL file that moves the schema forward, and the `schema_migrations` table records which revisions have been applied.

If you need to undo the effects of a migration, write a new migration that reverses the changes.

### Writing a Reverse Migration

Suppose you previously added a column:

```sql
-- Application/Migration/1700000000-add-slug-to-posts.sql
ALTER TABLE posts ADD COLUMN slug TEXT;
```

To undo this, generate a new migration and write the reverse:

```bash
new-migration "Remove slug from posts"
```

```sql
-- Application/Migration/1700100000-remove-slug-from-posts.sql
ALTER TABLE posts DROP COLUMN slug;
```

For more involved changes, here are the common reverse operations:

| Original Statement | Reverse Statement |
|---|---|
| `CREATE TABLE x (...)` | `DROP TABLE x` |
| `ALTER TABLE x ADD COLUMN y ...` | `ALTER TABLE x DROP COLUMN y` |
| `ALTER TABLE x ADD CONSTRAINT c ...` | `ALTER TABLE x DROP CONSTRAINT c` |
| `CREATE INDEX idx ON x (...)` | `DROP INDEX idx` |
| `ALTER TABLE x RENAME COLUMN a TO b` | `ALTER TABLE x RENAME COLUMN b TO a` |

Note that dropping a column or table permanently deletes the data in it. There is no way to recover the data from SQL alone. If you might need the data later, consider backing up the table first:

```sql
CREATE TABLE posts_slug_backup AS SELECT id, slug FROM posts;
ALTER TABLE posts DROP COLUMN slug;
```

### Test Migrations on a Copy First

Before running a migration against your production database, test it on a copy:

```bash
# Create a copy of your production database
pg_dump DATABASE_URL_PRODUCTION | psql DATABASE_URL_STAGING

# Run migrations against the copy
DATABASE_URL=DATABASE_URL_STAGING migrate
```

This lets you verify that the migration applies cleanly and that your application still works with the new schema.

## Data Migrations

Most migrations change the database schema -- adding tables, columns, or constraints. Sometimes you also need to transform existing data. These are called data migrations.

### Schema Migrations vs. Data Migrations

A **schema migration** changes the structure of the database:

```sql
ALTER TABLE posts ADD COLUMN slug TEXT;
```

A **data migration** changes the contents of existing rows:

```sql
UPDATE posts SET slug = LOWER(REPLACE(title, ' ', '-'));
```

In practice, you often need both in a single migration file.

### Combining Schema and Data Changes

A common pattern is adding a new column, populating it from existing data, and then making it non-nullable. Here is a complete example that adds a `slug` column to a `posts` table and fills it from the `title` column:

```sql
-- Step 1: Add the column as nullable (so existing rows are not rejected)
ALTER TABLE posts ADD COLUMN slug TEXT DEFAULT NULL;

-- Step 2: Populate the column from existing data
UPDATE posts SET slug = LOWER(REPLACE(title, ' ', '-')) WHERE slug IS NULL;

-- Step 3: Now that all rows have a value, make the column non-nullable
ALTER TABLE posts ALTER COLUMN slug SET NOT NULL;

-- Step 4: Optionally add a unique constraint
ALTER TABLE posts ADD CONSTRAINT posts_slug_key UNIQUE (slug);
```

This four-step pattern -- add nullable, backfill, set not-null, add constraints -- is the safest way to introduce a required column on a table that already has data.

### Another Example: Splitting a Name Column

Suppose you want to split a `name` column into `first_name` and `last_name`:

```sql
-- Add the new columns
ALTER TABLE users ADD COLUMN first_name TEXT DEFAULT NULL;
ALTER TABLE users ADD COLUMN last_name TEXT DEFAULT NULL;

-- Populate from the existing name column
-- This assumes names are in "First Last" format
UPDATE users SET
    first_name = SPLIT_PART(name, ' ', 1),
    last_name = CASE
        WHEN POSITION(' ' IN name) > 0 THEN SUBSTRING(name FROM POSITION(' ' IN name) + 1)
        ELSE ''
    END;

-- Make the new columns non-nullable
ALTER TABLE users ALTER COLUMN first_name SET NOT NULL;
ALTER TABLE users ALTER COLUMN last_name SET NOT NULL;

-- Drop the old column (only after deploying the code changes that use the new columns)
-- ALTER TABLE users DROP COLUMN name;
```

Notice that the `DROP COLUMN` is commented out. It is often safer to drop the old column in a separate, later migration after you have confirmed that the application code no longer references it.

## Development Workflow

### Schema.sql Is the Source of Truth

In IHP, `Application/Schema.sql` is the canonical definition of your database schema. It contains the full `CREATE TABLE`, `CREATE TYPE`, and other DDL statements that describe your database.

Migrations, on the other hand, are the incremental steps that bring an existing production database from an older version of the schema to the current one. Think of it this way:

- **Schema.sql**: "What the database looks like now"
- **Migrations**: "How to get there from an older version"

When you set up a fresh development database (e.g., with `make db`), IHP imports `Schema.sql` directly. Migrations are not used. Migrations are only used to update existing databases -- typically your production or staging databases.

### Schema Designer vs. Hand-Written SQL

The Schema Designer (available at `http://localhost:8001/Tables` when the dev server is running) provides a visual interface for editing `Schema.sql`. It is the recommended way to make schema changes during development because it:

- Keeps the SQL formatting consistent
- Automatically manages foreign key constraints
- Provides visual feedback about your schema

However, there are cases where hand-writing SQL in `Schema.sql` is more practical:

- Complex constraints or expressions that the visual editor does not support
- Bulk changes across many tables
- Adding comments or custom SQL functions

Either way, `Schema.sql` is always what gets modified. The Schema Designer just provides a UI for editing it.

### The Typical Workflow

Here is the recommended workflow for making database changes:

1. **Edit the schema**: Use the Schema Designer (or edit `Schema.sql` by hand) to make your changes. For example, add a new `slug` column to the `posts` table.

2. **Update your local database**: Click `Update DB` in the Schema Designer, or run `make db` from the command line. This re-creates your local development database from `Schema.sql` and `Fixtures.sql`.

3. **Generate a migration**: Open the Code Generator and click `Migration`. IHP will compare your `Schema.sql` against your local database and generate the SQL needed to bring an older database up to date. Alternatively, run `new-migration "Add slug to posts"` from the command line and write the SQL yourself.

4. **Review the migration**: Open the generated file in `Application/Migration/` and verify the SQL is correct. The auto-generated migration is a good starting point, but you may need to adjust it -- for example, to add a data backfill step.

5. **Test the migration locally**: Run `migrate` against your local database to verify it applies cleanly. If it fails, fix the SQL and try again.

6. **Deploy**: Push your code. In your deployment pipeline, run `migrate` before starting the new version of your application.

### When Your Development Database Gets Out of Sync

During development, your local database can get out of sync with `Schema.sql`. This typically happens when you:

- Edit `Schema.sql` without running `Update DB`
- Switch git branches that have different schemas
- Manually alter the database with `psql`

The easiest fix is to re-create the database from scratch:

```bash
make db
```

This drops the database, re-creates it from `Schema.sql`, and imports `Fixtures.sql`. All existing data in your local database will be lost, so make sure to run `make dumpdb` first if you want to preserve it.

If you want to keep your data and just apply the schema differences, use `Update DB` from the Schema Designer. It will dump your current data to `Fixtures.sql`, re-create the database, and re-import the data.

## Keeping Fixtures in Sync

`Application/Fixtures.sql` contains `INSERT INTO` statements that pre-populate your development database with sample data. When you run `make db`, IHP imports `Schema.sql` first and then `Fixtures.sql`.

### Why Fixtures Need Updating

When you change the schema, the `INSERT INTO` statements in `Fixtures.sql` may become invalid. For example:

- If you add a new `NOT NULL` column without a default, existing `INSERT` statements will fail because they do not include the new column.
- If you rename or drop a column, `INSERT` statements that reference the old column name will fail.

### How to Update Fixtures

The simplest approach is to run `make dumpdb` after updating your local database. This dumps the current database contents into `Fixtures.sql`, which will reflect the new schema.

```bash
# 1. Update your schema
# 2. Update DB (via Schema Designer or `make db`)
# 3. Manually insert or fix any sample data in the running database
# 4. Dump the updated database
make dumpdb
```

You can also edit `Fixtures.sql` by hand to add the new columns to your existing `INSERT` statements.

### Committing Fixtures

It is good practice to commit `Fixtures.sql` to version control. This way, other developers on your team get the same sample data when they set up their local database. After making schema changes, remember to update and commit `Fixtures.sql` alongside `Schema.sql` and the new migration file.

## Migration Best Practices

### Keep Migrations Small and Focused

Each migration should do one logical thing. Instead of a single migration that adds three tables, two indexes, and a data backfill, split it into separate migrations. Small migrations are easier to review, test, and debug.

```
Application/Migration/
    1700000000-create-categories-table.sql
    1700000001-create-posts-table.sql
    1700000002-add-slug-to-posts.sql
```

### Never Modify Migrations That Have Been Run in Production

Once a migration has been applied to a production database, treat it as immutable. The `schema_migrations` table records the migration's revision number, so `migrate` will not re-run it even if you change the file contents.

If you need to fix something introduced by an earlier migration, write a new migration with the corrective changes. This ensures that every database -- whether it ran the original migration a month ago or is being set up fresh today -- ends up in the same state.

### Always Test Migrations Locally

Before deploying, run your migration against a local database to catch syntax errors, constraint violations, and other issues:

```bash
migrate
```

For extra confidence, test against a copy of your production database (see "Test Migrations on a Copy First" above).

### Use Transactions for Safety

IHP wraps each migration in a `BEGIN` / `COMMIT` transaction by default. This means that if any statement in the migration fails, all changes are rolled back and the database is left in its previous state. You do not need to add `BEGIN` / `COMMIT` yourself.

The one exception is `ALTER TYPE ... ADD VALUE` for enums, which PostgreSQL does not allow inside a transaction. See the "ALTER TYPE ... ADD cannot run inside a transaction block" section above for the workaround.

### Add Default Values When Adding Non-Nullable Columns

If you add a `NOT NULL` column to a table that already has rows, the migration will fail unless you provide a default:

```sql
-- This will fail if the table has existing rows:
ALTER TABLE posts ADD COLUMN slug TEXT NOT NULL;

-- This works:
ALTER TABLE posts ADD COLUMN slug TEXT NOT NULL DEFAULT '';
```

Or use the nullable-then-backfill pattern described in the Data Migrations section above.

### Be Careful When Dropping Columns

Before dropping a column, make sure no running code references it. A safe approach is to deploy the code changes first (so the column is no longer read or written), and then deploy the migration that drops the column in a subsequent release.

### Order Matters for Dependent Objects

When creating tables with foreign key references, make sure the referenced table exists before the referencing table. Similarly, when dropping tables, drop the referencing table first:

```sql
-- Correct order for creation
CREATE TABLE users (...);
CREATE TABLE posts (
    ...
    user_id UUID NOT NULL REFERENCES users (id)
);

-- Correct order for deletion
DROP TABLE posts;
DROP TABLE users;
```

### Document Complex Migrations

For migrations that go beyond a simple `ALTER TABLE`, add SQL comments explaining the intent:

```sql
-- Split the full "name" column into separate first and last name columns.
-- Assumes names are in "First Last" format. Names without a space will have
-- an empty last_name, which should be corrected manually.
ALTER TABLE users ADD COLUMN first_name TEXT;
ALTER TABLE users ADD COLUMN last_name TEXT;
UPDATE users SET first_name = SPLIT_PART(name, ' ', 1), last_name = SPLIT_PART(name, ' ', 2);
ALTER TABLE users ALTER COLUMN first_name SET NOT NULL;
ALTER TABLE users ALTER COLUMN last_name SET NOT NULL;
```
