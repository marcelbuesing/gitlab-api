{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Data.Rest.SystemHooks (
    NewSystemHookItem(..)
  , SystemHookItem(..)
  ) where

import Data.Aeson
import Data.Text as T
import Network.URL (URL(..), exportURL, importURL)

instance FromJSON URL where
  parseJSON (String t) = case importURL $ T.unpack t of
    Just a -> pure a
    Nothing -> fail "failed to parse URL"
  parseJSON _ = fail "URL must be a text"

instance ToJSON URL where
  toJSON u = toJSON $ exportURL u

data SystemHookItem = SystemHookItem
  { _systemHookItemId :: Int
  , _systemHookItemUrl :: URL
  , _systemHookItemCreatedAt :: Text
  , _systemHookItemPushEvents :: Bool
  , _systemHookItemTagPushEvents :: Bool
  , _systemHookItemEnableSSLVerification :: Bool
  }

instance FromJSON SystemHookItem where
  parseJSON (Object v) = SystemHookItem <$>
    v .: "id" <*>
    v .: "url" <*>
    v .: "created_at" <*>
    v .: "push_events" <*>
    v .: "tag_push_events" <*>
    v .: "enable_ssl_verification"

data NewSystemHookItem = NewSystemHookItem
  { _newSystemHookItemUrl :: URL
  , _newSystemHookItemToken :: Text
  , _newSystemHookItemPushEvents :: Bool
  , _newSystemHookItemTagPushEvents :: Bool
  , _newSystemHookItemEnableSSLVerification :: Bool
  }

instance ToJSON NewSystemHookItem where
  toJSON (NewSystemHookItem u t pe tpe esv) =
    object
    [ "url" .= u
    , "token" .= t
    , "push_events" .= pe
    , "tag_push_events" .= tpe
    , "enable_ssl_verification" .= esv
    ]
