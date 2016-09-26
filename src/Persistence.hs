{-# LANGUAGE OverloadedStrings #-}
module Persistence where

import Database.PostgreSQL.Simple
import qualified Data.ByteString.Char8     as BSC

import Types

getUser :: BSC.ByteString -> Name -> IO User
getUser cs (Name name) = do
  conn <- connectPostgreSQL cs
  [(un,ui)] <- query conn "select * from user where name = ?" (Only name)
  return $ User (Name un) (InternalId ui)
