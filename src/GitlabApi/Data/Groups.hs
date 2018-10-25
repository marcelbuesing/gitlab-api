{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module GitlabApi.Data.Groups (
    ListGroupProject(..),
    Namespace(..)
  ) where

import Control.Lens.TH (makeLenses)
import Data.Aeson
import Data.Text as T
import GitlabApi.Data.ApiTypes

data Namespace = Namespace {
    _namespaceId :: Int
  , _namespaceName :: Text
  , _namespacePath :: Text
  , _namespaceKind :: Text
} deriving Show

makeLenses ''Namespace

instance FromJSON Namespace where
  parseJSON (Object v) = Namespace <$>
      v .: "id" <*>
      v .: "name" <*>
      v .: "path" <*>
      v .: "kind"


data ListGroupProject = ListGroupProject {
    _listGroupProjectId :: GroupsId
  , _listGroupProjectDescription :: Maybe Text
  , _listGroupProjectDefaultBranch :: Maybe Text
  , _listGroupProjectTagList :: [Text]
  , _listGroupProjectArchived :: Bool
  , _listGroupProjectVisibility :: Text
  , _listGroupProjectSshUrlToRepo :: Text
  , _listGroupProjectHttpUrlToRepo :: Text
  , _listGroupProjectWebUrl :: Text
  , _listGroupProjectName :: Text
  , _listGroupProjectNameWithNamespace :: Text
  , _listGroupProjectPath :: Text
  , _listGroupProjectPathWithNamespace :: Text
  , _listGroupProjectIssuesEnabled :: Bool
  , _listGroupProjectMergeRequestsEnabled :: Bool
  , _listGroupProjectWikiEnabled :: Bool
  , _listGroupProjectJobsEnabled :: Bool
  , _listGroupProjectSnippetsEnabled :: Bool
  , _listGroupProjectCreatedAt :: Text
  , _listGroupProjectLastActivityAt :: Text
  , _listGroupProjectSharedRunnersEnabled :: Bool
  , _listGroupProjectCreatorId :: Int
  , _listGroupProjectNamespace :: Namespace
  , _listGroupProjectAvatarUrl :: Maybe Text
  , _listGroupProjectStarCount :: Int
  , _listGroupProjectForksCount :: Int
  , _listGroupProjectOpenIssuesCount ::  Int
  , _listGroupProjectPublicJobs :: Bool
  , _listGroupProjectRequestAccessEnabled :: Bool
} deriving Show

makeLenses ''ListGroupProject

instance FromJSON ListGroupProject where
    parseJSON (Object v) = ListGroupProject <$>
        v .: "id" <*>
        v .: "description" <*>
        v .: "default_branch" <*>
        v .: "tag_list" <*>
        v .: "archived" <*>
        v .: "visibility" <*>
        v .: "ssh_url_to_repo" <*>
        v .: "http_url_to_repo" <*>
        v .: "web_url" <*>
        v .: "name" <*>
        v .: "name_with_namespace" <*>
        v .: "path" <*>
        v .: "path_with_namespace" <*>
        v .: "issues_enabled" <*>
        v .: "merge_requests_enabled" <*>
        v .: "wiki_enabled" <*>
        v .: "jobs_enabled" <*>
        v .: "snippets_enabled" <*>
        v .: "created_at" <*>
        v .: "last_activity_at" <*>
        v .: "shared_runners_enabled" <*>
        v .: "creator_id" <*>
        v .: "namespace" <*>
        v .: "avatar_url" <*>
        v .: "star_count" <*>
        v .: "forks_count" <*>
        v .: "open_issues_count" <*>
        v .: "public_jobs" <*>
        v .: "request_access_enabled"