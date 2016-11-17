{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Data.Commits (
    CommitSingle(..)
  , CommitStats(..)
  ) where

import GitlabApi.Data.ApiTypes
import Data.Aeson
import Data.Text as T
import Text.Email.Validate (EmailAddress, emailAddress)

data CommitSingle = CommitSingle
  { _commitSingleId :: CommitRef
  , _commitSingleShortId :: CommitRefShort
  , _commitSingleTitle :: Text
  , _commitSingleAuthorName :: Text
  , _commitSingleAuthorEmail :: EmailAddress
  , _commitSingleCreatedAt :: GitlabDate
  , _commitSingleMessage :: Text
  , _commitSingleCommittedDate :: GitlabDate
  , _commitSingleAuthoredDate :: GitlabDate
  , _commitSingleParentIds :: [CommitRef]
  , _commitSingleStats :: CommitStats
  , _commitSingleStatus :: Text -- TODO proper type
  }

instance FromJSON CommitSingle where
  parseJSON (Object v) = CommitSingle <$>
    v .: "id" <*>
    v .: "short_id" <*>
    v .: "title" <*>
    v .: "author_name" <*>
    v .: "author_email" <*>
    v .: "created_at" <*>
    v .: "message" <*>
    v .: "committed_date" <*>
    v .: "authored_date" <*>
    v .: "parent_ids" <*>
    v .: "stats" <*>
    v .: "status"

data CommitStats = CommitStats
  { _commitStatsAdditions :: Int
  , _commitStatsDeletions :: Int
  , _commitStatsTotal :: Int
  }

instance FromJSON CommitStats where
  parseJSON (Object v) = CommitStats <$>
    v .: "additions" <*>
    v .: "deletions" <*>
    v .: "total"
