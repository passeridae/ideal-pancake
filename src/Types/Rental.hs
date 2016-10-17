{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Types.Rental where

import           Data.Time
import           GHC.Generics
import           Prelude hiding (id)

import           Types.Common
import           Types.Copy
import           Types.User

--------------------------------------------------------------------------------

-- Rental

data Rental = Rental
  { id         :: InternalId Rental
  , copyId     :: InternalId Copy
  , userId     :: InternalId User
  , returnDate :: Day
  } deriving (Generic, Show)

--------------------------------------------------------------------------------
