{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Types.Copy where

import           Data.Aeson
import           Data.Proxy
import           Data.Text    (Text)
import           Data.UUID.V4
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Prelude hiding (id)
import           Servant.Docs hiding (notes)

import           Types.Book
import           Types.Common
import           Types.User

--------------------------------------------------------------------------------

-- Copy

data Copy = Copy
  { id       :: InternalId Copy
  , bookIsbn :: ISBN
  , notes    :: Notes
  } deriving (Generic, Show)


instance FromRow Copy where
  fromRow = Copy <$> field <*> field <*> (Notes <$> field) 

instance ToRow Copy where
  toRow Copy{..} = toRow (id, bookIsbn, unNotes $ notes)

--------------------------------------------------------------------------------

-- AddCopy

data AddCopyRequest = AddCopyRequest
  { notes :: Notes
  } deriving (Generic, Show)

instance ToJSON AddCopyRequest where
  toJSON = genericToJSON defaultAeson

instance FromJSON AddCopyRequest where
  parseJSON = genericParseJSON defaultAeson

instance ToSample AddCopyRequest where
  toSamples _ = do
    note <- snd <$> toSamples Proxy
    samples $ return $ AddCopyRequest note

acrToCopy :: ISBN -> AddCopyRequest -> IO Copy
acrToCopy isbn AddCopyRequest{..} = do
  copyId <- InternalId <$> nextRandom
  return $ Copy copyId isbn notes 

data AddCopyResponse = AddCopyResponse
  { id         :: Maybe (InternalId Copy)
  , successful :: Bool
  } deriving (Generic, Show, Eq, Ord)

instance ToJSON AddCopyResponse where
  toJSON = genericToJSON defaultAeson

instance FromJSON AddCopyResponse where
  parseJSON = genericParseJSON defaultAeson

instance ToSample AddCopyResponse where
  toSamples _ = do
    (_, mIid) <- toSamples Proxy
    let resp = case mIid of
          Nothing  -> ("Book doesn't exist within library", AddCopyResponse Nothing False)
          Just iid -> ("Add copy success", AddCopyResponse (Just iid) True)
    return resp

--------------------------------------------------------------------------------

-- Notes

newtype Notes = Notes
  { unNotes :: Maybe Text
  } deriving (Generic, Show, Ord, Eq, FromJSON, ToJSON)

instance ToSample Notes where
  toSamples _ = samples $ map Notes [Just "Damaged back cover", Just "Coffee stains throughout", Nothing]

--------------------------------------------------------------------------------

