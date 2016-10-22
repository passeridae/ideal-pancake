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
import           Database.PostgreSQL.Simple.ToField
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

newtype InternalId a = InternalId
  { unInternalId :: UUID
  } deriving (FromField, ToField, Generic, Eq, Ord, Show, Read)

instance ToJSON (InternalId a) where
  toJSON InternalId{..} = String (toText unInternalId)

instance FromJSON (InternalId a) where
  parseJSON = withText "InternalId" $ \x ->
    case fromText x of
      Nothing -> fail "Invalid UUID"
      Just u  -> return $ InternalId u

instance FromHttpApiData (InternalId a) where
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

instance ToSample (InternalId a) where
  toSamples _ = do
    let ids = unsafePerformIO $ replicateM 10 nextRandom--singleSample (InternalId (fromJust $ fromString "0fac788a-51bb-453e-a14a-61d70df8781d"))
    samples (map InternalId ids)

--------------------------------------------------------------------------------

-- Name

newtype Name = Name Text
  deriving (FromField, Generic, Eq, Ord, Show, IsString, FromJSON, ToJSON)

instance ToSample Name where
  toSamples _ = samples $ map Name ["Oswyn Brent", "Emily Olorin", "Tristram Healy", "Andrew Semler"]

instance FromHttpApiData Name where
  parseUrlPiece piece = Name <$> parseUrlPiece piece
  parseQueryParam param = Name <$> parseQueryParam param

instance ToParam (QueryParam "name" Name) where
  toParam _ = DocQueryParam "name" [] "name or name fragment to search for" Normal

instance ToParam (QueryParam "title" Name) where
  toParam _ = DocQueryParam "title" [] "title or title fragment to search for" Normal

--------------------------------------------------------------------------------

-- TagName

newtype TagName = TagName Text
  deriving (FromField, ToField, Generic, Eq, Ord, Show, IsString, FromJSON, ToJSON)

instance ToSample TagName where
  toSamples _ = samples $ map TagName ["sci-fi","fantasy", "depressing russian literature"]

instance FromHttpApiData TagName where
  parseUrlPiece piece = TagName <$> parseUrlPiece piece
  parseQueryParam param = TagName <$> parseQueryParam param

instance ToParam (QueryParam "tag" TagName) where
  toParam _ = DocQueryParam "tag" [] "tag or tag fragment to search for" Normal

--------------------------------------------------------------------------------
