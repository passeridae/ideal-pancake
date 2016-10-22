{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Persistence where

import           Control.Monad
import           Data.FileEmbed
import           Data.Pool
import           Data.Text                  (Text)
import           Data.Time
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
  searchBooksByTitle    :: Conn t -> Text -> m [Book]
  getAllBooks           :: Conn t -> m [Book]
  deleteBook            :: Conn t -> ISBN -> m ()
  addCopy               :: Conn t -> Copy -> m Bool
  updateCopy            :: Conn t -> InternalId Copy -> Notes -> m ()
  deleteCopy            :: Conn t -> InternalId Copy -> m ()
  getCopiesByIsbn       :: Conn t -> ISBN -> m [Copy]
  getCopyById           :: Conn t -> InternalId Copy -> m (Maybe Copy)
  addRental             :: Conn t -> Rental -> m ()
  getRental             :: Conn t -> InternalId Rental -> m (Maybe Rental)
  getRentalsByUser      :: Conn t -> InternalId User -> m [Rental]
  getCurrentRentalByCopy :: Conn t -> InternalId Copy -> m (Maybe Rental)
  completeRental        :: Conn t -> InternalId Rental -> Day -> m ()
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
    query conn "SELECT * FROM users WHERE LOWER (name) LIKE '%' || LOWER (? || '%'" (Only searchTerm)
  getAllUsers       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM users"
  deleteUser        (PGConn pool) (InternalId userId) = withResource pool $ \conn -> void $
    execute conn "DELETE FROM users where internalId = ?" (Only userId)
  -- | Books
  addBook           (PGConn pool) book = withResource pool $ \conn -> void $
    execute conn "INSERT into books (isbn,title,authors,publishers,yearOfPublication) VALUES (?,?,?,?,?)" book
  searchBooksByTitle (PGConn pool) title = withResource pool $ \conn ->
    query conn "SELECT * FROM books WHERE LOWER (title) LIKE '%' || LOWER (?) || '%'" (Only title)
  getBookByIsbn     (PGConn pool) isbn = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM books WHERE isbn = ?" (Only isbn)
  getAllBooks       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM books"
  deleteBook        (PGConn pool) isbn = do
      copies  <- getCopiesByIsbn (PGConn pool) isbn
      forM_ copies $ \Copy{..} -> do
        withResource pool $ \conn -> execute conn "DELETE FROM rentals WHERE copyId = ?" (Only id)
        withResource pool $ \conn -> execute conn "DELETE FROM copies  WHERE copyId = ?" (Only id)
      withResource pool $ \conn -> execute conn "DELETE FROM books where bookId = ?" (Only isbn)
      return ()
  addCopy           (PGConn pool) copy = withResource pool $ \conn -> do
    _ <- execute conn "INSERT into copies (copyId, copyOf, copyNotes) VALUES (?,?,?)" copy
    return True
  updateCopy        (PGConn pool) copyId notes = withResource pool $ \conn -> void $
    execute conn "UPDATE copies SET copyNotes = ? WHERE copyId = ? " (notes, copyId)
  deleteCopy        (PGConn pool) copyId = withResource pool $ \conn -> void $
    execute conn "DELETE FROM copies where copyId = ?" (Only copyId)
  getCopiesByIsbn   (PGConn pool) isbn = withResource pool $ \conn ->
    query conn "SELECT * FROM copies WHERE copyOf = ?" (Only isbn)
  getCopyById       (PGConn pool) copyId = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM copies WHERE copyId = ?" (Only copyId)
  addRental         (PGConn pool) rental = withResource pool $ \conn -> void $
    execute conn "INSERT into rentals (rentalId, copyId, userId, dueDate, returnDate) VALUES (?,?,?,?,?)" rental
  getRental         (PGConn pool) rentalId = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM rentals WHERE rentalId = ?" (Only rentalId)
  getRentalsByUser  (PGConn pool) userId = withResource pool $ \conn ->
    query conn "SELECT * FROM rentals WHERE userId = ?" (Only userId)  
  getCurrentRentalByCopy (PGConn pool) copyId = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM rentals WHERE copyId = ? AND returnDate is NULL" (Only copyId)
  completeRental (PGConn pool) rentalId returnDate = withResource pool $ \conn -> void $
    execute conn "UPDATE rentals SET returnDate = ? where rentalId = ?" (returnDate, rentalId)
  addReservation        (PGConn pool) reservation = withResource pool $ \conn -> void $
    execute conn "INSERT into reservations (reservationId, reserveOf, userId, requestDate) VALUES (?,?,?,?)" reservation
  getReservationsByIsbn (PGConn pool) isbn = withResource pool $ \conn -> 
    query conn "SELECT * FROM reservations WHERE reserveOf = ?" (Only isbn)
  getReservationsByUser (PGConn pool) userId = withResource pool $ \conn -> 
    query conn "SELECT * FROM reservations WHERE userId = ?" (Only userId)
  addTag                (PGConn pool) tag = withResource pool $ \conn -> void $
    execute conn "INSERT into tags (tagName, tagNotes) VALUES (?,?)" tag
  getTags               (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM tags"
  getTagByName          (PGConn pool) tagName = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM tags WHERE tagName = ?" (Only tagName)
  searchTagsByName      (PGConn pool) searchTerm = withResource pool $ \conn ->
    query conn "SELECT * FROM tags WHERE LOWER(tagName) LIKE '%' || LOWER(?) || '%'" (Only searchTerm)
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
