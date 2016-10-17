{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types.User where

import           Data.Aeson
import           Data.Proxy
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Prelude                            hiding (id)
import           Servant.Docs                       hiding (notes)

import           Types.Common                       as X

--------------------------------------------------------------------------------

-- User

data User = User
  { name :: Name
  , id   :: InternalId User
  } deriving (Generic, Eq, Ord, Show)

instance ToJSON User where
  toJSON = genericToJSON defaultAeson

instance ToSample User where
  toSamples _ = do
    (_, id)   <- toSamples Proxy
    (_, name) <- toSamples Proxy
    samples $ return User{..}

instance FromRow User where
  fromRow = User <$> field <*> field

instance ToRow User where
  toRow (User (Name name) (InternalId iid)) = toRow (name, iid)

--------------------------------------------------------------------------------

-- AddUser

data AddUserRequest = AddUserRequest
  { name :: Name
  } deriving (Generic, Show, Ord, Eq)

instance ToJSON AddUserRequest where
  toJSON = genericToJSON defaultAeson

instance FromJSON AddUserRequest where
  parseJSON = genericParseJSON defaultAeson

instance ToSample AddUserRequest where
  toSamples _ = do
    (_, name) <- toSamples Proxy
    samples $ return $ AddUserRequest name

data AddUserResponse = AddUserResponse
  { id :: InternalId User
  } deriving (Generic, Show, Ord, Eq)

instance ToJSON AddUserResponse where
  toJSON = genericToJSON defaultAeson

instance FromJSON AddUserResponse where
  parseJSON = genericParseJSON defaultAeson

instance ToSample AddUserResponse where
  toSamples _ = do
    (_, iid) <- toSamples Proxy
    samples $ return $ AddUserResponse iid

--------------------------------------------------------------------------------

-- DeleteUser

data DeleteUserRequest
data DeleteUserResponse

--------------------------------------------------------------------------------

-- UpdateUser

data UpdateUserRequest
data UpdateUserResponse

--------------------------------------------------------------------------------
