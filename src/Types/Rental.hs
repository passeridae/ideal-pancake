{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types.Rental where

import           Control.Arrow
import           Data.Aeson
import           Data.Proxy
import           Data.Text                          (Text)
import           Data.Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Prelude                            hiding (id)
import qualified Prelude                            (id)
import           Servant.Docs

import           Types.Common
import           Types.Copy
import           Types.User

--------------------------------------------------------------------------------

-- Rental

data Rental = Rental
  { id         :: InternalId Rental
  , copyId     :: InternalId Copy
  , userId     :: InternalId User
  , dueDate    :: Day
  , returnDate :: Maybe Day
  } deriving (Generic, Show)

instance FromRow Rental where
  fromRow = Rental <$> field <*> field <*> field <*> field <*> field

instance ToRow Rental where
  toRow Rental{..} = toRow (id, copyId, userId, dueDate, returnDate)
--------------------------------------------------------------------------------

-- RentCopy

data RentalRequest = RentalRequest
  { copyId     :: InternalId Copy
  , userId     :: InternalId User
  , dueDate    :: Day
  } deriving (Generic, Show)

instance ToJSON RentalRequest where
  toJSON = genericToJSON defaultAeson

instance FromJSON RentalRequest where
  parseJSON = genericParseJSON defaultAeson

instance ToSample RentalRequest where
  toSamples _ = do
    (copyId, userId) <- snd <$> toSamples Proxy
    samples $ return $ RentalRequest copyId userId (fromGregorian 2016 11 31)

data RentalResponse = RentalResponse
  { id         :: Maybe (InternalId Rental)
  , successful :: Bool
  } deriving (Generic, Show, Eq, Ord)

instance ToJSON RentalResponse where
  toJSON = genericToJSON defaultAeson

instance FromJSON RentalResponse where
  parseJSON = genericParseJSON defaultAeson

instance ToSample RentalResponse where
  toSamples _ = do
    (_, mIid) <- toSamples Proxy
    let resp = case mIid of
          Nothing  -> ("Copy already on loan", RentalResponse Nothing False)
          Just iid -> ("Rental success", RentalResponse (Just iid) True)
    return resp
--------------------------------------------------------------------------------

-- CompleteRental

data CompleteRentalRequest = CompleteRentalRequest
  { rentalId :: InternalId Rental
  } deriving (Generic, Show)

instance ToJSON CompleteRentalRequest where
  toJSON = genericToJSON defaultAeson

instance FromJSON CompleteRentalRequest where
  parseJSON = genericParseJSON defaultAeson

instance ToSample CompleteRentalRequest where
  toSamples _ = do
    rentalId <- snd <$> toSamples Proxy
    samples $ return $ CompleteRentalRequest rentalId

data CompleteRentalResponse = NoSuchRental
                            | RentalAlreadyComplete
                            | CompleteRentalSuccess
   deriving (Generic, Show, Eq, Ord, Bounded, Enum)

completeRentalResponseMessage :: CompleteRentalResponse -> Text
completeRentalResponseMessage = \case
    NoSuchRental          -> "Rental doesn't exist"
    RentalAlreadyComplete -> "Rental is already complete"
    CompleteRentalSuccess -> "Complete rental success"

instance ToJSON CompleteRentalResponse where
  toJSON crr =
    object [ "message" .= completeRentalResponseMessage crr
           , "successful" .= (crr == CompleteRentalSuccess) ]

instance ToSample CompleteRentalResponse where
  toSamples _ = map (completeRentalResponseMessage &&& Prelude.id) $ enumFrom minBound

--------------------------------------------------------------------------------
