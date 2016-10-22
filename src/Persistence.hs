{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Persistence where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.FileEmbed
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as M
import           Data.Monoid
import           Data.Pool
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Database.PostgreSQL.Simple
import           Prelude                    hiding (id)

import           Types

-- | t is the phantom type for which the associated types `Conn t` and `Conf t`
--   are from.
class Monad m => Store t m where
  data Conn t :: *
  data Conf t :: *

  initConnection        :: Conf t -> m (Conn t)
  destroyConnection     :: Conn t -> m ()
  initStore             :: Conn t -> m ()
  addUser               :: Conn t -> User -> m ()
  getUserById           :: Conn t -> InternalId User -> m (Maybe User)
  deleteUser            :: Conn t -> InternalId User -> m ()
  searchUsersByName     :: Conn t -> Text -> m [User]
  getAllUsers           :: Conn t -> m [User]
  addBook               :: Conn t -> Book -> m ()
  getBookByIsbn         :: Conn t -> ISBN -> m (Maybe Book)
  getAllBooks           :: Conn t -> m [Book]
  addCopy               :: Conn t -> Copy -> m Bool
  getCopiesByIsbn       :: Conn t -> ISBN -> m [Copy]
  addRental             :: Conn t -> Rental -> m ()
  getRental             :: Conn t -> InternalId Rental -> m (Maybe Rental)
  getRentalsByUser      :: Conn t -> InternalId User -> m [Rental]
  getRentalsByCopy      :: Conn t -> InternalId Copy -> m [Rental]
  addReservation        :: Conn t -> Reservation -> m ()
  getReservationsByIsbn :: Conn t -> ISBN -> m [Reservation]
  getReservationsByUser :: Conn t -> InternalId user -> m [Reservation]
  addTag                :: Conn t -> Tag -> m ()
  getTags               :: Conn t -> m [Tag]
  searchTagsByName      :: Conn t -> Text -> m [Tag]
  getTagByName          :: Conn t -> TagName -> m (Maybe Tag)
  addBookTag            :: Conn t -> BookTag -> m ()
  getBooksByTag         :: Conn t -> TagName -> m [ISBN]


data Postgres

defaultPostgres :: Conf Postgres
defaultPostgres = PGConf $ defaultConnectInfo { connectDatabase = "idealpancake" }

safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

instance Store Postgres IO where
  data Conn Postgres = PGConn (Pool Connection)
  data Conf Postgres = PGConf ConnectInfo

   -- | 1 stripe, 5 second unused connection lifetime, 4 max connections to DB
  initConnection    (PGConf connInfo) = PGConn <$> createPool (connect connInfo) close 1 5 4
  destroyConnection (PGConn pool) = destroyAllResources pool
  initStore         (PGConn pool) = withResource pool $ \conn -> void $ execute_ conn initSql
  -- | Users
  addUser           (PGConn pool) user = withResource pool $ \conn ->
    void $ execute conn "INSERT into users (name,internalId) VALUES (?,?)" user
  getUserById       (PGConn pool) (InternalId internalId) = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM users WHERE internalId = ?" (Only internalId)
  searchUsersByName (PGConn pool) searchTerm = withResource pool $ \conn ->
    query conn "SELECT * FROM users WHERE name LIKE '%' || ? || '%'" (Only searchTerm)
  getAllUsers       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM users"
  deleteUser        (PGConn pool) (InternalId userId) = withResource pool $ \conn -> void $
    execute conn "DELETE FROM users where internalId = ?" (Only userId)
  -- | Books
  addBook           (PGConn pool) book = withResource pool $ \conn -> void $
    execute conn "INSERT into books (isbn,title,authors,publishers,yearOfPublication) VALUES (?,?,?,?,?)" book
  getBookByIsbn     (PGConn pool) isbn = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM books WHERE isbn = ?" (Only isbn)
  getAllBooks       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM books"
  addCopy           (PGConn pool) copy = withResource pool $ \conn -> do
    _ <- execute conn "INSERT into copies (copyId, copyOf, copyNotes) VALUES (?,?,?)" copy
    return True
  getCopiesByIsbn   (PGConn pool) isbn = withResource pool $ \conn ->
    query conn "SELECT * FROM copies WHERE copyOf = ? " (Only isbn)
  addRental         (PGConn pool) rental = withResource pool $ \conn -> void $
    execute conn "INSERT into rentals (rentalId, copyId, userId, returnDate) VALUES (?,?,?,?)" rental
  getRental         (PGConn pool) rentalId = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM rentals WHERE rentalId = ?" (Only rentalId)
  getRentalsByUser  (PGConn pool) userId = withResource pool $ \conn ->
    query conn "SELECT * FROM rentals WHERE userId = ?" (Only userId)
  getRentalsByCopy  (PGConn pool) copyId = withResource pool $ \conn ->
    query conn "SELECT * FROM rentals WHERE copyId = ?" (Only copyId)
  addReservation        (PGConn pool) reservation = withResource pool $ \conn -> void $
    execute conn "INSERT into reservations (reservationId, reserveOf, userId, requestDate) VALUES (?,?,?,?)" reservation
  getReservationsByIsbn (PGConn pool) isbn = withResource pool $ \conn -> 
    query conn "SELECT * FROM reservations WHERE reserveOf = ?" (Only isbn)
  getReservationsByUser (PGConn pool) userId = withResource pool $ \conn -> 
    query conn "SELECT * FROM reservations WHERE userId = ?" (Only userId)
  addTag                (PGConn pool) tag = withResource pool $ \conn -> void $
    execute conn "INSERT into tags (tagName, tagNotes) VALUES (?,?)" tag
  getTagByName          (PGConn pool) tagName = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM tags WHERE tagName = ?" (Only tagName)
  searchTagsByName      (PGConn pool) searchTerm = withResource pool $ \conn ->
    query conn "SELECT * FROM tags WHERE tagName ? LIKE '%' || ? || '%'" (Only searchTerm)
  addBookTag            (PGConn pool) booktag@BookTag{..} = withResource pool $ \conn -> void $ do
    tr <- getTagByName (PGConn pool) tagName
    case tr of 
      Nothing -> addTag (PGConn pool) (Tag tagName Nothing)
      _ -> return ()
    execute conn "INSERT into booktags (tagOf, tagName) VALUES (?,?)" booktag
  getBooksByTag         (PGConn pool) tagName = withResource pool $ \conn ->
    (map (\(BookTag isbn _) -> isbn)) <$> query conn "SELECT * FROM booktags where tagName = ?" (Only tagName) 

initSql :: Query
initSql = $(embedStringFile "database/db.sql")
