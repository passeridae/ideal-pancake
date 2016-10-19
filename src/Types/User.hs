{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DataKinds                  #-}

module Types.User where

import           Control.Arrow
import           Data.Aeson
import           Data.Proxy
import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Prelude                            hiding (id)
import qualified Prelude                            (id)
import           Servant.Docs                       hiding (notes)
import           Servant hiding (NoSuchUser)


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

instance ToCapture (Capture "user_id" (InternalId User)) where
  toCapture _ = DocCapture "user_id" "unique identifier for the user"

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

-- UpdateUser

-- Same data as an add user request
type UpdateUserRequest = AddUserRequest

data UpdateUserResponse = NoSuchUser
                        | UpdateUserSuccess
   deriving (Generic, Show, Eq, Ord, Bounded, Enum)

updateUserResponseMessage :: UpdateUserResponse -> Text
updateUserResponseMessage = \case
    NoSuchUser        -> "User doesn't exist"
    UpdateUserSuccess -> "User update success"

instance ToJSON UpdateUserResponse where
  toJSON crr =
    object [ "message" .= updateUserResponseMessage crr
           , "successful" .= (crr == UpdateUserSuccess) ]

instance ToSample UpdateUserResponse where
  toSamples _ = map (updateUserResponseMessage &&& Prelude.id) $ enumFrom minBound



--------------------------------------------------------------------------------
