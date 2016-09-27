{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Types where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Proxy
import           Data.String       hiding (fromString)
import           Data.Text         (Text)
import           Data.Time
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics
import           Servant.Docs
import           System.IO.Unsafe

type ISBN = Text
type Author = Text
type Publisher = Text
type Title = Text

newtype Name = Name Text
  deriving (Generic, Eq, Ord, Show, IsString, ToJSON)

instance ToSample Name where
  toSamples _ = samples $ map Name ["Oswyn Brent", "Emily Olorin", "Tristram Healy", "Andrew Semler"]

newtype InternalId = InternalId
  { unInternalId :: UUID
  } deriving (Generic, Eq, Ord, Show)

instance ToJSON InternalId where
  toJSON InternalId{..} = String (toText unInternalId)

instance ToSample InternalId where
  toSamples _ = do
    let ids = unsafePerformIO $ replicateM 10 nextRandom--singleSample (InternalId (fromJust $ fromString "0fac788a-51bb-453e-a14a-61d70df8781d"))
    samples (map InternalId ids)

data CopyStatus = Available
                | OnLoan User
  deriving (Generic, Show)

data Book = Book
  { isbn              :: ISBN
  , title             :: Title
  , authors           :: [Author]
  , publishers        :: [Publisher]
  , yearOfPublication :: UTCTime
  } deriving (Generic, Show)

instance ToJSON Book where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance ToSample Book where
  toSamples _ = let now = unsafePerformIO getCurrentTime
                in singleSample $ Book "lol-legit-isbn" "A Story of Sadness" ["Emily Olorin", "Oswyn Brent"] ["Sadness Publishing"] now

data Copy = Copy
  { copyOf     :: ISBN
  , copyId     :: InternalId
  , copyNotes  :: Maybe Text
  , copyStatus :: CopyStatus
  } deriving (Generic, Show)

data AddCopyRequest = AddCopyRequest
  { acrBook  :: ISBN
  , acrNotes :: Maybe Text
  } deriving (Generic, Show)

acrToCopy :: AddCopyRequest -> IO Copy
acrToCopy AddCopyRequest{..} = do
  copyId <- nextRandom
  return $ Copy acrBook (InternalId copyId) acrNotes Available

data AddUserRequest
data AddUserResponse
data DeleteUserRequest
data DeleteUserResponse
data UpdateUserRequest
data UpdateUserResponse

data User = User
  { name   :: Name
  , userId :: InternalId
  } deriving (Generic, Eq, Ord, Show)

instance ToJSON User where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase

instance ToSample User where
  toSamples _ = do
    (_, name)       <- toSamples Proxy
    (_, identifier) <- toSamples Proxy
    samples $ return (User name identifier)
