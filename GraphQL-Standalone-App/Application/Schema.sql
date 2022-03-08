-- Your database schema. Use the Schema Designer at http://localhost:8001/ to add some tables.
CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    email TEXT NOT NULL,
    password_hash TEXT NOT NULL,
    locked_at TIMESTAMP WITH TIME ZONE DEFAULT NULL,
    failed_login_attempts INT DEFAULT 0 NOT NULL
);
CREATE TABLE tasks (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    title TEXT NOT NULL,
    body TEXT NOT NULL,
    user_id UUID NOT NULL
);
CREATE POLICY "Allow access" ON tasks USING (true) WITH CHECK (true);
ALTER TABLE tasks ENABLE ROW LEVEL SECURITY;
CREATE INDEX tasks_user_id_index ON tasks (user_id);
ALTER TABLE tasks ADD CONSTRAINT tasks_ref_user_id FOREIGN KEY (user_id) REFERENCES users (id) ON DELETE NO ACTION;
