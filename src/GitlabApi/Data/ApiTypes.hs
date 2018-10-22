{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Data.ApiTypes where

import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Maybe(fromJust)
import Data.Monoid((<>))
import Data.Text as T
import Network.URL (URL(..), exportURL, importURL)
import Text.Email.Validate (EmailAddress, emailAddress)

type AuthorId = Int
type AssigneeId = Int

type GitlabDate = Text

type BuildId = Int
type CommitRef = Text
type CommitRefShort = Text

type FileName = Text
type FileSize = Int

type ObjectAttributeId = Int
type ObjectAttributeIid = Int

type MilestoneId = Int

-- | User's name
type Name = Text

type ProjectId =  Int
data ProjectVisibility = Private | Public | Internal deriving Show
type ProjectKey = Text

type GroupsId = Int

instance FromJSON ProjectVisibility where
  parseJSON (String "private") = return  Private
  parseJSON (String "public") = return Public
  parseJSON (String "internal") = return Internal

type ProjectVisibilityLevel = Int

type RepositoryVisibilityLevel = Int

type UserId = Int
type Username = Text

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

data Project = Project
  { _projectName :: Text
  , _projectDescription :: Text
  , _projectWebUrl :: URL
  , _projectAvatarUrl :: Maybe URL
  , _projectGitSshUrl :: URL
  , _projectGitHttpUrl :: URL
  , _projectNamespace :: Text
  , _projectVisibilityLevel :: ProjectVisibilityLevel
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

-- | You can use a personal access token to authenticate with the API by passing it
-- | in either the private_token parameter or the Private-Token header.
newtype PrivateToken = PrivateToken { _unToken :: T.Text } deriving Show

data GitlabInstance = GitlabInstance
  {
    -- | example https://gitlab.example.com
    _gitlabInstanceUrl :: URL
    -- | example 9koXpg98eAheJpvBs5tK
  , _gitlabInstancePrivateToken :: PrivateToken
  } deriving Show

-- smart constructor
gitlabInstance :: URL -> PrivateToken -> GitlabInstance
gitlabInstance url token = GitlabInstance (fromJust (importURL ((exportURL url) <> "api/v4"))) token
