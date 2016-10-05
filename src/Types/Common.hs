{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Types.Common where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Types
import           Data.String                          hiding (fromString)
import           Data.Text                            (Text)
import           Data.UUID
import           Data.UUID.V4
import           Database.PostgreSQL.Simple.FromField hiding (name)
import           GHC.Generics
import           Servant
import           Servant.Docs
import           System.IO.Unsafe

--------------------------------------------------------------------------------

-- Aeson util

defaultAeson :: Options
defaultAeson = (aesonDrop 0 snakeCase){omitNothingFields = True}

--------------------------------------------------------------------------------

-- Internal Identifiers (UUID v4)

newtype InternalId = InternalId
  { unInternalId :: UUID
  } deriving (FromField, Generic, Eq, Ord, Show)

instance ToJSON InternalId where
  toJSON InternalId{..} = String (toText unInternalId)

instance FromJSON InternalId where
  parseJSON = withText "InternalId" $ \x ->
    case fromText x of
      Nothing -> fail "Invalid UUID"
      Just u  -> return $ InternalId u

instance FromHttpApiData InternalId where
  parseUrlPiece piece = do
    text <- parseUrlPiece piece
    case fromText text of
      Nothing -> fail "Invalid UUID"
      Just u  -> return $ InternalId u
  parseQueryParam param = do
    text <- parseQueryParam param
    case fromText text of
      Nothing -> fail "Invalid UUID"
      Just u  -> return $ InternalId u

instance ToSample InternalId where
  toSamples _ = do
    let ids = unsafePerformIO $ replicateM 10 nextRandom--singleSample (InternalId (fromJust $ fromString "0fac788a-51bb-453e-a14a-61d70df8781d"))
    samples (map InternalId ids)

instance ToCapture (Capture "user_id" InternalId) where
  toCapture _ = DocCapture "user_id" "unique identifier for the user"


--------------------------------------------------------------------------------

-- Name

newtype Name = Name Text
  deriving (FromField, Generic, Eq, Ord, Show, IsString, FromJSON, ToJSON)

instance ToSample Name where
  toSamples _ = samples $ map Name ["Oswyn Brent", "Emily Olorin", "Tristram Healy", "Andrew Semler"]

--------------------------------------------------------------------------------
