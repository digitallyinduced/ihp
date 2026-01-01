CREATE TABLE users (
    id uuid PRIMARY KEY,
    name text NOT NULL,
    email text NOT NULL
);

CREATE TABLE posts (
    id uuid PRIMARY KEY,
    author_id uuid REFERENCES users(id),
    slug text NOT NULL
);
