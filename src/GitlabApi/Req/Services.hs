{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Req.Services where

import Data.ByteString.Lazy(ByteString)
import Data.Monoid((<>))
import Network.Wreq
import Network.URL (exportURL)
import Control.Lens
import Data.Aeson
import Data.Text as T
import Data.Text.Encoding(encodeUtf8)
import GHC.Generics
import GitlabApi.Data.ApiTypes
import GitlabApi.Data.Services(GetJira(..), PutJira(..))
import GitlabApi.Req.Common

getJiraService :: GitlabInstance -> ProjectId -> IO (Maybe GetJira)
getJiraService gitlab project_id = do
    let opts = privateTokenHeader (_gitlabInstancePrivateToken gitlab)
        url = exportURL (_gitlabInstanceUrl gitlab) <> "/projects/" <> show project_id <> "/services/jira"
    r <- asJSON =<< getWith opts url
    return $ r ^? responseBody

createJiraService :: GitlabInstance -> ProjectId -> PutJira -> IO (Response ByteString)
createJiraService gitlab project_id jira = putWith opts url (toJSON jira)
    where url = exportURL (_gitlabInstanceUrl gitlab) <> "/projects/" <> show project_id <> "/services/jira"
          opts = privateTokenHeader (_gitlabInstancePrivateToken gitlab)

editJiraService :: GitlabInstance -> ProjectId -> PutJira -> IO (Response ByteString)
editJiraService = createJiraService

deleteJiraService :: GitlabInstance -> Int -> IO (Response ByteString)
deleteJiraService gitlab project_id = deleteWith opts url
  where url = exportURL (_gitlabInstanceUrl gitlab) <> "/projects/" <> show project_id <> "/services/jira.org/post"
        opts = privateTokenHeader (_gitlabInstancePrivateToken gitlab)