{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GitlabApi.Data.Services (
    GetJira(..),
    PutJira(..)
  ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text as T
import GitlabApi.Data.ApiTypes
import Network.URL (URL(..), exportURL)

data GetJira = GetJira {
    _getJiraId :: Maybe Int
  , _getJiraTitle :: Text
  , _getJiraCreatedAt :: Maybe Text
  , _getJiraUpdatedAt :: Maybe Text
  , _getJiraActive :: Bool
  , _getJiraPushEvents :: Bool
  , _getJiraIssueEvents :: Bool
  , _getJiraConfidentialIssuesEvents :: Bool
  , _getJiraMergeRequestsEvents :: Bool
  , _getJiraTagPushEvents :: Bool
  , _getJiraNoteEvents :: Bool
  , _getJiraJobEvents :: Bool
  , _getJiraPipelineEvents :: Bool
  -- properties
} deriving Show

makeLenses ''GetJira

instance FromJSON GetJira where
    parseJSON (Object v) = GetJira <$>
      v .: "id" <*>
      v .: "title" <*>
      v .: "created_at" <*>
      v .: "updated_at" <*>
      v .: "active" <*>
      v .: "push_events" <*>
      v .: "issues_events" <*>
      v .: "confidential_issues_events" <*>
      v .: "merge_requests_events" <*>
      v .: "tag_push_events" <*>
      v .: "note_events" <*>
      v .: "job_events" <*>
      v .: "pipeline_events"


data PutJira = PutJira {
    _putJiraUrl        :: URL
  , _putJiraProjectKey :: ProjectKey
  , _putJiraUsername   :: Text
  , _putJiraPassword   :: Text
  , _putJiraIssueTransitionId :: Maybe Int
} deriving Show

makeLenses ''PutJira

instance ToJSON PutJira where
    toJSON (PutJira url pk u pass jiti) =
        object
        [ "url" .= exportURL url
        , "project_key" .= pk
        , "username" .= u
        , "password" .= pass
        , "jira_issue_transition_id" .= jiti
        ]