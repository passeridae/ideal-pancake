{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Persistence where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Map                   (Map)
import qualified Data.Map.Strict            as M
import           Data.Pool
import           Database.PostgreSQL.Simple
import           Data.Vector(Vector)
import qualified Data.Vector as V

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

-- | Example InMemory implementation
data InMemory
-- | Empty X data decl
data X

instance Store InMemory STM where
  data Conn InMemory = InMemoryVar (TVar (Map InternalId User))
  data Conf InMemory = X

  -- | No fancy connection stuff required
  initConnection _ = InMemoryVar <$> newTVar mempty
  destroyConnection _ = pure ()
  initStore _ = pure ()

  addUser     (InMemoryVar var) user  = modifyTVar var (M.insert (userId user) user)
  getUserById    (InMemoryVar var) ident = M.lookup ident <$> readTVar var
  getUserByName = undefined
  getAllUsers (InMemoryVar var)       = M.elems <$> readTVar var

data Postgres


safeHead :: [a] -> Maybe a
safeHead (x:_) = Just x
safeHead _ = Nothing

instance Store Postgres IO where
  data Conn Postgres = PGConn (Pool Connection)
  data Conf Postgres = PGConf ConnectInfo

   -- | 1 stripe, 5 second unused connection lifetime, 4 max connections to DB
  initConnection (PGConf connInfo) = PGConn <$> createPool (connect connInfo) close 1 5 4
  destroyConnection (PGConn pool) = destroyAllResources pool
  initStore (PGConn pool) = withResource pool $ \conn -> void $ execute_ conn initSql
  addUser (PGConn pool) user = withResource pool $ \conn -> void $ execute conn "insert into users (name,id) values (?,?)" user
  getUserById (PGConn pool) (InternalId internalId) = withResource pool $ \conn -> do
    users <- query conn "select * from users where internalId = ?" (Only internalId)
    return $ safeHead users  
  getUserByName (PGConn pool) (Name un) = withResource pool $ \conn -> do
    users <- query conn "select * from users where name = ?" (Only un)
    return $ safeHead users  
  getAllUsers (PGConn pool) = withResource pool $ \conn -> do
    query_ conn "select * from users"
  addBook (PGConn pool) book = withResource pool $ \conn -> void $
    execute conn "insert into books (isbn,title,authors,publishers,yearOfPublication) values (?,?,?,?,?)" book
  getBookByIsbn (PGConn pool) isbn = withResource pool $ \conn -> do
    books <- query conn "select * from books where isbn = ?" (Only isbn)
    return $ safeHead books 
  getAllBooks (PGConn pool) = undefined  
  
-- | TODO: Implement
--   Source a file, template in the sql, whatever
initSql :: Query
initSql = "create table users (name varchar 200,internalId UUID) ;"
