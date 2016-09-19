{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Types where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Text         (Text)
import           Data.Time         (UTCTime)
import           Data.UUID
import           Data.UUID.V4
import           GHC.Generics

type ISBN = Text
type Author = Text
type Publisher = Text
type Title = Text

newtype InternalId = InternalId
  { unInternalId :: UUID
  } deriving (Generic, Show)

instance ToJSON InternalId where
  toJSON InternalId{..} = String (toText unInternalId)

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
acrToCopy req@AddCopyRequest{..} = do
  copyId <- nextRandom
  return $ Copy acrBook (InternalId copyId) acrNotes Available

data User = User
  { name   :: Text
  , userId :: InternalId
  } deriving (Generic, Show)

instance ToJSON User where
  toJSON = genericToJSON $ aesonDrop 0 snakeCase
