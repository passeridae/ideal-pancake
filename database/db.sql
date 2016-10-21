CREATE TABLE IF NOT EXISTS users (name text NOT NULL, internalId uuid PRIMARY KEY);
CREATE TABLE IF NOT EXISTS books (isbn text PRIMARY KEY, title text NOT NULL, authors text[] NOT NULL, publishers text[] NOT NULL, yearOfPublication timestamp NOT NULL);
CREATE TABLE IF NOT EXISTS tags (tagName text PRIMARY KEY, tagNotes text);
CREATE TABLE IF NOT EXISTS booktags (bookTagId uuid PRIMARY KEY, tagOf text REFERENCES books (isbn), tagName text REFERENCES tags (tagName));
CREATE TABLE IF NOT EXISTS copies (copyId uuid PRIMARY KEY, copyOf text REFERENCES books (isbn) NOT NULL,  copyNotes text);
CREATE TABLE IF NOT EXISTS rentals (rentalId uuid PRIMARY KEY, copyId uuid REFERENCES copies (copyId), userId uuid REFERENCES users (internalId), returnDate timestamp NOT NULL); 
CREATE TABLE IF NOT EXISTS reservations (reservationId uuid PRIMARY KEY, reserveOf text REFERENCES books (isbn), userId uuid REFERENCES users (internalId), requestDate timestamp NOT NULL); 
