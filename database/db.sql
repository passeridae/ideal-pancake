CREATE TABLE IF NOT EXISTS users (name text NOT NULL, internalId uuid PRIMARY KEY);
CREATE TABLE IF NOT EXISTS books (isbn text PRIMARY KEY, title text NOT NULL, authors text[] NOT NULL, publishers text[] NOT NULL, yearOfPublication timestamp NOT NULL );
CREATE TABLE IF NOT EXISTS copies (copyOf REFERENCES books (isbn) NOT NULL, copyId uuid PRIMARY KEY, copyNotes text, copyStatus text NOT NULL);
