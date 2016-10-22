{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Types.Tag where

import           Data.Text                          (Text)
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToRow
import           GHC.Generics
import           Prelude                            hiding (id)
import qualified Prelude                            (id)

import           Types.Common
import           Types.Book

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
  { tagOf       :: ISBN
  , tagName     :: TagName
  } deriving (Generic, Show)

instance FromRow BookTag where
  fromRow = BookTag <$> field <*> field

instance ToRow BookTag where
  toRow (BookTag tagOf (TagName tn))  = toRow (tagOf, tn)
--------------------------------------------------------------------------------
