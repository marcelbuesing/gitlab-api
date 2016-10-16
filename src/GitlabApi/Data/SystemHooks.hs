{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Data.SystemHooks (
    SystemHook(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.HashMap.Lazy as HML
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
  , _userCreatedName :: Name
  , _userCreatedUserName :: Username
  , _userCreatedUserId :: UserId
  } |
  UserRemoved
  { _userRemovedCreatedAt :: GitlabDate
  , _userRemovedUpdatedAtEvent :: GitlabDate
  , _userRemovedEmail :: EmailAddress
  , _userRemovedEventName :: Text
  , _userRemovedName :: Name
  , _userRemovedUserName :: Username
  , _userRemovedUserId :: UserId
  } |
  SHPushEvent
  { _shPushEventBefore :: CommitRef
  , _shPushEventAfter :: CommitRef
  , _shPushEventRef :: Text
  , _shPushEventCheckoutSha :: Text
  , _shPushEventUserId :: Int
  , _shPushEventUserName :: Username
  , _shPushEventUserEmail :: EmailAddress
  , _shPushEventUserAvatar:: Text
  , _shPushEventProjectId :: ProjectId
  , _shPushEventTotalCommitsCount :: Int
  , _shPushEventProject :: Project
  , _shPushEventCommits :: [Commit]
  , _shPushEventRepository :: PushEventRepository
  }

instance FromJSON SystemHook where
  parseJSON (Object v) = case HML.lookup "event_name" v of
    Just (String "project_create") -> parseProjectCreated v
    Just (String "project_destroy") -> parseProjectDestroyed v
    Just (String "project_rename") -> parseProjectRenamed v
    Just (String "project_transfer") -> parseProjectTransferred v
    Just (String "user_create") -> parseUserCreated v
    Just (String "user_destroy") -> parseUserDestroyed v
    Just (String "push") -> parsePushEvent v
    _ -> fail "unexpected event"

parseProjectCreated :: Object -> Parser SystemHook
parseProjectCreated v = ProjectCreated <$>
  v .: "created_at" <*>
  v .: "updated_at" <*>
  v .: "event_name" <*>
  v .: "name" <*>
  v .: "owner_email" <*>
  v .: "owner_name" <*>
  v .: "path" <*>
  v .: "path_with_namespace" <*>
  v .: "project_id" <*>
  v .: "project_visibility"

parseProjectDestroyed :: Object -> Parser SystemHook
parseProjectDestroyed v = ProjectDestroyed <$>
  v .: "created_at" <*>
  v .: "updated_at" <*>
  v .: "event_name" <*>
  v .: "name" <*>
  v .: "owner_email" <*>
  v .: "owner_name" <*>
  v .: "path" <*>
  v .: "path_with_namespace" <*>
  v .: "project_id" <*>
  v .: "project_visibility"

parseProjectRenamed :: Object -> Parser SystemHook
parseProjectRenamed v = ProjectRenamed <$>
  v .: "created_at" <*>
  v .: "updated_at" <*>
  v .: "event_name" <*>
  v .: "name" <*>
  v .: "owner_email" <*>
  v .: "owner_name" <*>
  v .: "path" <*>
  v .: "path_with_namespace" <*>
  v .: "old_path_with_namespace" <*>
  v .: "project_id" <*>
  v .: "project_visibility"

parseProjectTransferred :: Object -> Parser SystemHook
parseProjectTransferred v = ProjectTransferred <$>
  v .: "created_at" <*>
  v .: "updated_at" <*>
  v .: "event_name" <*>
  v .: "name" <*>
  v .: "owner_email" <*>
  v .: "owner_name" <*>
  v .: "path" <*>
  v .: "path_with_namespace" <*>
  v .: "old_path_with_namespace" <*>
  v .: "project_id" <*>
  v .: "project_visibility"

parseUserCreated :: Object -> Parser SystemHook
parseUserCreated v = UserCreated <$>
  v .: "created_at" <*>
  v .: "updated_at" <*>
  v .: "email" <*>
  v .: "event_name" <*>
  v .: "name" <*>
  v .: "username" <*>
  v .: "user_id"

parseUserDestroyed :: Object -> Parser SystemHook
parseUserDestroyed v = UserRemoved <$>
  v .: "created_at" <*>
  v .: "updated_at" <*>
  v .: "email" <*>
  v .: "event_name" <*>
  v .: "name" <*>
  v .: "username" <*>
  v .: "user_id"

parsePushEvent :: Object -> Parser SystemHook
parsePushEvent v = SHPushEvent <$>
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
