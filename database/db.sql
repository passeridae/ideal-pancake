CREATE TABLE IF NOT EXISTS users (name text NOT NULL, internalId uuid PRIMARY KEY);
CREATE TABLE IF NOT EXISTS books (isbn text PRIMARY KEY, title text NOT NULL, authors text[] NOT NULL, publishers text[] NOT NULL, yearOfPublication timestamp NOT NULL );
CREATE TABLE IF NOT EXISTS copies (copyId uuid PRIMARY KEY, copyOf REFERENCES books (isbn) NOT NULL,  copyNotes text);
CREATE TABLE IF NOT EXISTS rentals (rentalId uuid PRIMARY KEY, copyId REFERENCES copies (copyId), userId REFERENCES users (internalId), returnDate timestamp NOR NULL); 
