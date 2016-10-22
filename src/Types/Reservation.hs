{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types.Reservation where

import           Data.Text                          (Text)
import           Data.Time
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Prelude                            hiding (id)
import qualified Prelude                            (id)

import           Types.Common
import           Types.Book
import           Types.User

--------------------------------------------------------------------------------

-- Reservation

data Reservation = Reservation
  { id          :: InternalId Reservation
  , reserveOf   :: ISBN
  , userId      :: InternalId User
  , requestDate :: UTCTime
  } deriving (Generic, Show)

instance FromRow Reservation where
  fromRow = Reservation <$> field <*> field <*> field <*> field

instance ToRow Reservation where
  toRow Reservation{..} = toRow (id, reserveOf, userId, requestDate)
--------------------------------------------------------------------------------
