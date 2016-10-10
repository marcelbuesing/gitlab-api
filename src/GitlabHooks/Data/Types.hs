{-# LANGUAGE OverloadedStrings #-}

module GitlabHooks.Data.Types where

import qualified Data.ByteString.Char8 as B
import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.HashMap.Lazy as HML
import Data.Text as T
import Network.URL (URL(..), importURL)
import Text.Email.Validate (EmailAddress, emailAddress)


instance FromJSON EmailAddress where
    parseJSON (String t) = case emailAddress $ B.pack . T.unpack $ t of
        Just a -> pure a
        Nothing -> fail "failed to parse EmailAddress"
    parseJSON _ = fail "EmailAddress must be a text"

instance FromJSON URL where
  parseJSON (String t) = case importURL $ T.unpack t of
    Just a -> pure a
    Nothing -> fail "failed to parse URL"
  parseJSON _ = fail "URL must be a text"

data GitlabEvent =
  PushEvent
  { _pushEventObjectKind :: Text
  , _pushEventBefore :: CommitRef
  , _pushEventAfter :: CommitRef
  , _pushEventRef :: Text
  , _pushEventCheckoutSha :: Text
  , _pushEventUserId :: Int
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
  , _issueEventObjectAttributes :: ObjectAttribute
  , _issueEventAssignee :: Assignee
  } deriving Show

instance FromJSON GitlabEvent where
  parseJSON (Object v) = case HML.lookup "object_kind" v of
    Just (String "push") -> parsePushEvent v
    Just (String "issue") -> parseIssueEvent v
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

type CommitRef = Text
type ProjectId = Int

type RepositoryVisibilityLevel = Int

data Project = Project
  { _projectName :: Text
  , _projectDescription :: Text
  , _projectWebUrl :: URL
  , _projectAvatarUrl :: Maybe URL
  , _projectGitSshUrl :: URL
  , _projectGitHttpUrl :: URL
  , _projectNamespace :: Text
  , _projectVisibilitylevel :: Int
  , _projectPathWithNamespace :: Text
  , _projectDefaultBranch :: Text
  , _projectHomepage :: URL
  , _projectUrl :: URL
  , _projectSshUrl :: URL
  , _projectHttpUrl :: URL
  } deriving Show

instance FromJSON Project where
  parseJSON (Object v) = Project <$>
    v .: "name" <*>
    v .: "description" <*>
    v .: "web_url" <*>
    v .: "avatar_url" <*>
    v .: "git_ssh_url" <*>
    v .: "git_http_url" <*>
    v .: "namespace" <*>
    v .: "visibility_level" <*>
    v .: "path_with_namespace" <*>
    v .: "default_branch" <*>
    v .: "homepage" <*>
    v .: "url" <*>
    v .: "ssh_url" <*>
    v .: "http_url"

data PushEventRepository = PushEventRepository
  { _pushEventRepositoryName :: Text
  , _pushEventRepositoryUrl :: URL
  , _pushEventRepositoryDescription :: Text
  , _pushEventRepositoryHomepage :: URL
  , _pushEventRepositoryGitHttpUrl :: URL
  , _pushEventRepositoryGitSshUrl :: URL
  , _pushEventRepositoryVisibilityLevel :: RepositoryVisibilityLevel
  } deriving Show

instance FromJSON PushEventRepository where
  parseJSON (Object v) = PushEventRepository <$>
    v .: "name" <*>
    v .: "url" <*>
    v .: "description" <*>
    v .: "homepage" <*>
    v .: "git_http_url" <*>
    v .: "git_ssh_url" <*>
    v .: "visibility_level"

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

data Commit = Commit
  { _commitId :: CommitRef
  , _commitMessage :: Text
  , _commitTimestamp :: Text
  , _commitUrl :: URL
  , _commitAuthor :: CommitAuthor
  , _commitAdded :: [Text]
  , _commitModified :: [Text]
  , _commitRemoved :: [Text]
  } deriving Show

instance FromJSON Commit where
  parseJSON (Object v) = Commit <$>
    v .: "id" <*>
    v .: "message" <*>
    v .: "timestamp" <*>
    v .: "url" <*>
    v .: "author" <*>
    v .: "added" <*>
    v .: "modified" <*>
    v .: "removed"

data CommitAuthor = CommitAuthor
  { _commitAuthorName :: Text
  , _commitAuthorEmail :: EmailAddress
  } deriving Show

instance FromJSON CommitAuthor where
  parseJSON (Object v) = CommitAuthor <$>
    v .: "name" <*>
    v .: "email"

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
  , _assigneeUsername :: Text
  , _assigneeAvatarUrl :: URL
  } deriving Show

instance FromJSON Assignee where
  parseJSON (Object v) = Assignee <$>
    v .: "name" <*>
    v .: "username" <*>
    v .: "avatar_url"

type ObjectAttributeId = Int
type AssigneeId = Int
type AuthorId = Int
type MilestoneId = Int
type ObjectAttributeState = Text
type ObjectAttributeIid = Int
type ObjectAttributeAction = Text

data ObjectAttribute = ObjectAttribute
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

instance FromJSON ObjectAttribute where
  parseJSON (Object v) = ObjectAttribute <$>
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
