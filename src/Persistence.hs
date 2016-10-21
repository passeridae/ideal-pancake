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
  getUsersByName        :: Conn t -> Text -> m [User]
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
  getTagByName          :: Conn t -> TagName -> m (Maybe Tag)
  addBookTag            :: Conn t -> BookTag -> m ()
  getBooksByTag         :: Conn t -> TagName -> m [ISBN]

-- | Example InMemory implementation
data InMemory
-- | Empty X data decl
data X

data InMemoryStore = InMemoryStore
  { users  :: TVar (Map (InternalId User) User)
  , books  :: TVar (Map ISBN Book)
  , copies :: TVar (Map ISBN [Copy])
  }

instance Store InMemory STM where
  data Conn InMemory = InMemoryVar InMemoryStore
  data Conf InMemory = X

  -- | No fancy connection stuff required
  initConnection _ = InMemoryVar <$> (InMemoryStore <$> newTVar mempty <*> newTVar mempty <*> newTVar mempty)
  destroyConnection _ = pure ()
  initStore _ = pure ()

  addUser        (InMemoryVar InMemoryStore{..}) user@User{..} = modifyTVar users (M.insert id user)
  getUserById    (InMemoryVar InMemoryStore{..}) ident         = M.lookup ident <$> readTVar users
  getUsersByName (InMemoryVar InMemoryStore{..}) searchTerm    = filter (\User{name=(Name name)} -> searchTerm `T.isInfixOf` name) . M.elems <$> readTVar users
  getAllUsers    (InMemoryVar InMemoryStore{..})               = M.elems <$> readTVar users
  addBook        (InMemoryVar InMemoryStore{..}) book          = modifyTVar books (M.insert (isbn book) book)
  getBookByIsbn  (InMemoryVar InMemoryStore{..}) ident         = M.lookup ident <$> readTVar books
  getAllBooks    (InMemoryVar InMemoryStore{..})               = M.elems <$> readTVar books
  addCopy        var@(InMemoryVar InMemoryStore{..}) copy@Copy{..} = do
    book <- getBookByIsbn var bookIsbn
    case book of
      Nothing -> pure False
      Just _  -> do
        modifyTVar copies $ M.insertWith (<>) bookIsbn [copy]
        pure True
  getCopiesByIsbn       = undefined
  addRental             = undefined
  getRental             = undefined
  getRentalsByCopy      = undefined
  getRentalsByUser      = undefined
  addReservation        = undefined 
  getReservationsByIsbn = undefined 
  getReservationsByUser = undefined 
  addTag                = undefined 
  getTagByName          = undefined 
  addBookTag            = undefined 
  getBooksByTag         = undefined 

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
  getUsersByName    (PGConn pool) searchTerm = withResource pool $ \conn ->
    query conn "SELECT * FROM users WHERE ? LIKE '%' || name || '%'" (Only searchTerm)
  getAllUsers       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM users"
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
  addBookTag            (PGConn pool) booktag = withResource pool $ \conn -> void $
    execute conn "INSERT into booktags (bookTagId, tagOf, tagName) VALUES (?,?,?)" booktag
  getBooksByTag         (PGConn pool) tagName = withResource pool $ \conn ->
    (map (\(BookTag _ isbn _) -> isbn)) <$> query conn "SELECT * FROM booktags where tagName = ?" (Only tagName) 

initSql :: Query
initSql = $(embedStringFile "database/db.sql")
