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
import           Database.PostgreSQL.Simple
import           Prelude                    hiding (id)

import           Types

-- | t is the phantom type for which the associated types `Conn t` and `Conf t`
--   are from.
class Monad m => Store t m where
  data Conn t :: *
  data Conf t :: *

  initConnection      :: Conf t -> m (Conn t)
  destroyConnection   :: Conn t -> m ()
  initStore           :: Conn t -> m ()
  addUser             :: Conn t -> User -> m ()
  getUserById         :: Conn t -> InternalId -> m (Maybe User)
  getUserByName       :: Conn t -> Name -> m (Maybe User)
  getAllUsers         :: Conn t -> m [User]
  addBook             :: Conn t -> Book -> m ()
  getBookByIsbn       :: Conn t -> ISBN -> m (Maybe Book)
  getAllBooks         :: Conn t -> m [Book]
  addCopy             :: Conn t -> Copy -> m Bool

-- | Example InMemory implementation
data InMemory
-- | Empty X data decl
data X

data InMemoryStore = InMemoryStore
  { users  :: TVar (Map InternalId User)
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

  addUser       (InMemoryVar InMemoryStore{..}) user@User{..} = modifyTVar users (M.insert id user)
  getUserById   (InMemoryVar InMemoryStore{..}) ident         = M.lookup ident <$> readTVar users
  getUserByName (InMemoryVar InMemoryStore{..}) name2         = safeHead . filter (\User{..} -> name == name2) . M.elems <$> readTVar users
  getAllUsers   (InMemoryVar InMemoryStore{..})               = M.elems <$> readTVar users
  addBook       (InMemoryVar InMemoryStore{..}) book          = modifyTVar books (M.insert (isbn book) book)
  getBookByIsbn (InMemoryVar InMemoryStore{..}) ident         = M.lookup ident <$> readTVar books
  getAllBooks   (InMemoryVar InMemoryStore{..})               = M.elems <$> readTVar books
  addCopy       var@(InMemoryVar InMemoryStore{..}) copy@Copy{..} = do
    book <- getBookByIsbn var bookIsbn
    case book of
      Nothing -> pure False
      Just _  -> do
        modifyTVar copies $ M.insertWith (<>) bookIsbn [copy]
        pure True

data Postgres


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
    void $ execute conn "INSERT into users (name,id) VALUES (?,?)" user
  getUserById       (PGConn pool) (InternalId internalId) = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM users WHERE internalId = ?" (Only internalId)
  getUserByName     (PGConn pool) (Name un) = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM users WHERE name = ?" (Only un)
  getAllUsers       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM users"
  -- | Books
  addBook           (PGConn pool) book = withResource pool $ \conn -> void $
    execute conn "INSERT into books (isbn,title,authors,publishers,yearOfPublication) VALUES (?,?,?,?,?)" book
  getBookByIsbn     (PGConn pool) isbn = withResource pool $ \conn ->
    safeHead <$> query conn "SELECT * FROM books WHERE isbn = ?" (Only isbn)
  getAllBooks       (PGConn pool) = withResource pool $ \conn ->
    query_ conn "SELECT * FROM books"
  addCopy = error "NYI"

initSql :: Query
initSql = $(embedStringFile "database/db.sql") 