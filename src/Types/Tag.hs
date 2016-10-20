{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types.Tag where

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
import           Types.Book
import           Types.User

--------------------------------------------------------------------------------

-- Tag

data Tag = Tag
  { tagName     :: TagName
  , tagNotes    :: Maybe Text
  } deriving (Generic, Show)

instance FromRow Tag where
  fromRow = Tag <$> field <*> field 

instance ToRow Tag where
  toRow (Tag (TagName tn) tagNotes) = toRow (tn, tagNotes)
--------------------------------------------------------------------------------

-- BookTag

data BookTag = BookTag
  { id          :: InternalId BookTag
  , tagOf       :: ISBN
  , tagName     :: TagName
  } deriving (Generic, Show)

instance FromRow BookTag where
  fromRow = BookTag <$> field <*> field <*> field

instance ToRow BookTag where
  toRow (BookTag id tagOf (TagName tn))  = toRow (id, tagOf, tn)
--------------------------------------------------------------------------------
