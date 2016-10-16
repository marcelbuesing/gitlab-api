{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Data.WebHooks where

import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.HashMap.Lazy as HML
import Data.Maybe (fromJust)
import Data.Text as T
import Network.URL (URL(..), importURL)
import Text.Email.Validate (EmailAddress, emailAddress)

import GitlabApi.Data.ApiTypes

data GitlabEvent =
  PushEvent
  { _pushEventObjectKind :: Text
  , _pushEventBefore :: CommitRef
  , _pushEventAfter :: CommitRef
  , _pushEventRef :: Text
  , _pushEventCheckoutSha :: Text
  , _pushEventUserId :: UserId
  , _pushEventUserName :: Text
  , _pushEventUserEmail :: EmailAddress
  , _pushEventUserAvatar:: Text
  , _pushEventProjectId :: ProjectId
  , _pushEventTotalCommitsCount :: Int
  , _pushEventProject :: Project
  , _pushEventCommits :: [Commit]
  , _pushEventRepository :: PushEventRepository
  }
  | IssueEvent
  { _issueEventObjectKind :: Text
  , _issueEventUser :: User
  , _issueEventProject :: Project
  , _issueEventRepository :: IssueEventRepository
  , _issueEventObjectAttributes :: IssueEventObjectAttribute
  , _issueEventAssignee :: Assignee
  }
  | PipelineEvent
  { _pipelineEventObjectKind :: Text
  , _pipelineEventObjectAttributes :: PipelineEventObjectAttribute
  , _pipelineEventUser :: User
  , _pipelineEventProject :: PipelineEventProject
  , _pipelineEventCommit :: PipelineEventCommit
  , _pipelineEventBuilds :: [Build]
  } deriving Show

instance FromJSON GitlabEvent where
  parseJSON (Object v) = case HML.lookup "object_kind" v of
    Just (String "push") -> parsePushEvent v
    Just (String "issue") -> parseIssueEvent v
    Just (String "pipeline") -> parsePipelineEvent v
    _ -> fail "unexpected event"

parsePushEvent :: Object -> Parser GitlabEvent
parsePushEvent v = PushEvent <$>
    v .: "object_kind" <*>
    v .: "before" <*>
    v .: "after" <*>
    v .: "ref" <*>
    v .: "checkout_sha" <*>
    v .: "user_id" <*>
    v .: "user_name" <*>
    v .: "user_email" <*>
    v .: "user_avatar" <*>
    v .: "project_id" <*>
    v .: "total_commits_count" <*>
    v .: "project" <*>
    v .: "commits" <*>
    v .: "repository"

parseIssueEvent :: Object -> Parser GitlabEvent
parseIssueEvent v = IssueEvent <$>
    v .: "object_kind" <*>
    v .: "user" <*>
    v .: "project" <*>
    v .: "repository" <*>
    v .: "object_attributes" <*>
    v .: "assignee"

parsePipelineEvent :: Object -> Parser GitlabEvent
parsePipelineEvent v = PipelineEvent <$>
  v .: "object_kind" <*>
  v .: "object_attributes" <*>
  v .: "user" <*>
  v .: "project" <*>
  v .: "commit" <*>
  v .: "builds"

data IssueEventRepository = IssueEventRepository
  { _issueEventRepositoryName :: Text
  , _issueEventRepositoryUrl :: URL
  , _issueEventRepositoryDescription :: Text
  , _issueEventRepositoryHomepage :: URL
  } deriving Show

instance FromJSON IssueEventRepository where
  parseJSON (Object v) = IssueEventRepository <$>
    v .: "name" <*>
    v .: "url" <*>
    v .: "description" <*>
    v .: "homepage"

data User = User
  { _userName :: Text
  , _userUserName :: Text
  , _userAvatarUrl :: Text
  } deriving Show

instance FromJSON User where
  parseJSON (Object v) = User <$>
    v .: "name" <*>
    v .: "username" <*>
    v .: "avatar_url"

data Assignee = Assignee
  { _assigneeName :: Text
  , _assigneeUsername :: Username
  , _assigneeAvatarUrl :: URL
  } deriving Show

instance FromJSON Assignee where
  parseJSON (Object v) = Assignee <$>
    v .: "name" <*>
    v .: "username" <*>
    v .: "avatar_url"


type ObjectAttributeState = Text
type ObjectAttributeAction = Text

data IssueEventObjectAttribute = IssueEventObjectAttribute
  { _objectAttributeId :: ObjectAttributeId
  , _objectAttributeTitle :: Text
  , _objectAttributeAssigneeId :: AssigneeId
  , _objectAttributeAuthorId :: AuthorId
  , _objectAttributeProjectId :: ProjectId
  , _objectAttributeCreatedAt :: Text
  , _objectAttributeUpdatedAt :: Text
  , _objectAttributePosition :: Int
  , _objectAttributeBranchname :: Maybe Text
  , _objectAttributeDescription :: Text
  , _objectAttributeMilestoneId :: Maybe MilestoneId
  , _objectAttributeState :: ObjectAttributeState
  , _objectAttributeIid :: ObjectAttributeIid
  , _objectAttributeUrl :: URL
  , _objectAttributeAction :: ObjectAttributeAction
  } deriving Show

instance FromJSON IssueEventObjectAttribute where
  parseJSON (Object v) = IssueEventObjectAttribute <$>
    v .: "id" <*>
    v .: "title" <*>
    v .: "assignee_id" <*>
    v .: "author_id" <*>
    v .: "project_id" <*>
    v .: "created_at" <*>
    v .: "updated_at" <*>
    v .: "position" <*>
    v .: "branch_name" <*>
    v .: "description" <*>
    v .: "milestone_id" <*>
    v .: "state" <*>
    v .: "iid" <*>
    v .: "url" <*>
    v .: "action"

type ObjectAttributeRef = Text
type SHA = Text
type PipelineEventStatus = Text
type PipelineEventStage = Text

data PipelineEventObjectAttribute = PipelineEventObjectAttribute
  { _pipelineEventObjectAttributeId :: ObjectAttributeId
  , _pipelineEventObjectAttributeRef :: ObjectAttributeRef
  , _pipelineEventObjectAuttributeTag :: Bool
  , _pipelineEventObjectAttributeSha :: SHA
  , _pipelineEventObjectAttributeBeforeSHA :: SHA
  , _pipelineEventObjectAttributeStatus :: PipelineEventStatus
  , _pipelineEventObjectAttributeStages :: [PipelineEventStage]
  , _pipelineEventObjectAttributeCreatedAt :: GitlabDate
  , _pipelineEventObjectAttributeFinishedAt :: GitlabDate
  , _pipelineEventObjectAttributeDuration :: Int
  } deriving Show

instance FromJSON PipelineEventObjectAttribute where
  parseJSON (Object v) = PipelineEventObjectAttribute <$>
    v .: "id" <*>
    v .: "ref" <*>
    v .: "tag" <*>
    v .: "sha" <*>
    v .: "before_sha" <*>
    v .: "status" <*>
    v .: "stages" <*>
    v .: "created_at" <*>
    v .: "finished_at" <*>
    v .: "duration"

data PipelineEventProject = PipelineEventProject
  { _pipelineEventProjectName :: Text
  , _pipelineEventProjectDescription :: Text
  , _pipelineEventProjectWebUrl :: URL
  , _pipelineEventProjectAvatarUrl :: Maybe URL
  , _pipelineEventProjectGitSshUrl :: URL
  , _pipelineEventProjectGitHttpUrl :: URL
  , _pipelineEventProjectNamespace :: Text
  , _pipelineEventProjectVisibilityLevel :: ProjectVisibilityLevel
  , _pipelineEventProjectPathWithNamespace :: Text
  , _pipelineEventProjectDefaultBranch :: Text
  } deriving Show

instance FromJSON PipelineEventProject where
  parseJSON (Object v) = PipelineEventProject <$>
    v .: "name" <*>
    v .: "description" <*>
    v .: "web_url" <*>
    v .: "avatar_url" <*>
    v .: "git_ssh_url" <*>
    v .: "git_http_url" <*>
    v .: "namespace" <*>
    v .: "visibility_level" <*>
    v .: "path_with_namespace" <*>
    v .: "default_branch"

data PipelineEventCommit = PipelineEventCommit
  { _pipeLineEventCommitId :: CommitRef
  , _pipeLineEventCommitMessage :: Text
  , _pipeLineEventCommitTimestamp :: Text
  , _pipeLineEventCommitUrl :: URL
  , _pipeLineEventCommitAuthor :: CommitAuthor
  } deriving Show

instance FromJSON PipelineEventCommit where
  parseJSON (Object v) = PipelineEventCommit <$>
    v .: "id" <*>
    v .: "message" <*>
    v .: "timestamp" <*>
    v .: "url" <*>
    v .: "author"

type BuildStage = Text
type BuildRunner = Text
type BuildStatus = Text

data Build = Build
  { _buildId :: BuildId
  , _buildStage :: BuildStage
  , _buildName :: Text
  , _buildStatus :: BuildStatus
  , _buildCreatedAt :: GitlabDate
  , _buildStartedAt :: Maybe GitlabDate
  , _buildFinishedAt :: Maybe GitlabDate
  , _buildWhen :: Text
  , _buildManual :: Bool
  , _buildUser :: User
  , _buildRunner :: Maybe BuildRunner
  , _buildArtifactsFile :: BuildArtifactsFile
  } deriving Show

instance FromJSON Build where
  parseJSON (Object v) = Build <$>
    v .: "id" <*>
    v .: "stage" <*>
    v .: "name" <*>
    v .: "status" <*>
    v .: "created_at" <*>
    v .: "started_at" <*>
    v .: "finished_at" <*>
    v .: "when" <*>
    v .: "manual" <*>
    v .: "user" <*>
    v .: "runner" <*>
    v .: "artifacts_file"

data BuildArtifactsFile = BuildArtifactsFile
  { _buildArtifactsFileFileName :: Maybe FileName
  , _buildArtifactsFileSize :: Maybe FileSize
  } deriving Show

instance FromJSON BuildArtifactsFile where
  parseJSON (Object v) = BuildArtifactsFile <$>
    v .: "filename" <*>
    v .: "size"
