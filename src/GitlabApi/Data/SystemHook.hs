module GitlabApi.Data.SystemHook where

import Data.Text as T
import Text.Email.Validate (EmailAddress, emailAddress)

import GitlabApi.Data.ApiTypes

data SystemHook =
  ProjectCreated
  { _projectCreatedCreatedAt :: GitlabDate
  , _projectCreatedUpdatedAtEvent :: GitlabDate
  , _projectCreatedEventName :: Text
  , _projectCreatedName :: Text
  , _projectCreatedOwnerEmail :: EmailAddress
  , _projectCreatedOwnerName :: Text
  , _projectCreatedPath :: Text
  , _projectCreatedPathWithNamespace :: Text
  , _projectCreatedProjectId :: ProjectId
  , _projectCreatedProjectVisibility :: ProjectVisibility
  } |
  ProjectDestroyed
  { _projectDestroyedCreatedAt :: GitlabDate
  , _projectDestroyedUpdatedAtEvent :: GitlabDate
  , _projectDestroyedEventName :: Text
  , _projectDestroyedName :: Text
  , _projectDestroyedOwnerEmail :: EmailAddress
  , _projectDestroyedOwnerName :: Text
  , _projectDestroyedPath :: Text
  , _projectDestroyedPathWithNamespace :: Text
  , _projectDestroyedProjectId :: ProjectId
  , _projectDestroyedProjectVisibility :: ProjectVisibility
  } |
  ProjectRenamed
  { _projectRenamedCreatedAt :: GitlabDate
  , _projectRenamedUpdatedAtEvent :: GitlabDate
  , _projectRenamedEventName :: Text
  , _projectRenamedName :: Text
  , _projectRenamedOwnerEmail :: EmailAddress
  , _projectRenamedOwnerName :: Text
  , _projectRenamedPath :: Text
  , _projectRenamedPathWithNamespace :: Text
  , _projectRenamedOldPathWithNamespace :: Text
  , _projectRenamedProjectId :: ProjectId
  , _projectRenamedProjectVisibility :: ProjectVisibility
  } |
  ProjectTransferred
  { _projectRenamedCreatedAt :: GitlabDate
  , _projectRenamedUpdatedAtEvent :: GitlabDate
  , _projectRenamedEventName :: Text
  , _projectRenamedName :: Text
  , _projectRenamedOwnerEmail :: EmailAddress
  , _projectRenamedOwnerName :: Text
  , _projectRenamedPath :: Text
  , _projectRenamedPathWithNamespace :: Text
  , _projectRenamedOldPathWithNamespace :: Text
  , _projectRenamedProjectId :: ProjectId
  , _projectRenamedProjectVisibility :: ProjectVisibility
  } |
  UserCreated
  { _userCreatedCreatedAt :: GitlabDate
  , _userCreatedUpdatedAtEvent :: GitlabDate
  , _userCreatedEmail :: EmailAddress
  , _userCreatedEventName :: Text
  , _userCreatedName ::  Username
  , _userCreatedUserId :: UserId
  } |
  UserRemoved
  { _userRemovedCreatedAt :: GitlabDate
  , _userRemovedUpdatedAtEvent :: GitlabDate
  , _userRemovedEmail :: EmailAddress
  , _userRemovedEventName :: Text
  , _userRemovedName ::  Username
  , _userRemovedUserId :: UserId
  } |
  PushEvent
  { _pushEventBefore :: CommitRef
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
