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


instance Store Postgres IO where
  data Conn Postgres = PGConn (Pool Connection)
  data Conf Postgres = PGConf ConnectInfo

   -- | 1 stripe, 5 second unused connection lifetime, 4 max connections to DB
  initConnection (PGConf connInfo) = PGConn <$> createPool (connect connInfo) close 1 5 4
  destroyConnection (PGConn pool) = destroyAllResources pool
  initStore (PGConn pool) = withResource pool $ \conn -> void $ execute_ conn initSql
  addUser (PGConn pool) (User (Name un) ui) = withResource pool $ \conn -> void $ execute conn "insert into users (name,id) values (?,?)" (un,unInternalId ui)
  getUserById (PGConn pool) (InternalId internalId) = withResource pool $ \conn -> do
    users <- query conn "select * from users where internalId = ?" (Only internalId)
    return $ case users of 
      (u:_) -> Just u
      [] -> Nothing
  getUserByName (PGConn pool) (Name un) = withResource pool $ \conn -> do
    users <- query conn "select * from users where name = ?" (Only un)
    return $ case users of 
      (u:_) -> Just u 
      [] -> Nothing
  getAllUsers (PGConn pool) = withResource pool $ \conn -> do
    query_ conn "select * from users"

-- | TODO: Implement
--   Source a file, template in the sql, whatever
initSql :: Query
initSql = undefined
