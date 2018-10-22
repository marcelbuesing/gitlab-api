{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Req.Groups where

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
import GitlabApi.Data.Groups(ListGroupProject(..))
import GitlabApi.Req.Common

listGroupProjects :: GitlabInstance -> ProjectId -> IO (Maybe [ListGroupProject])
listGroupProjects gitlab project_id = do
    let opts = privateTokenHeader (_gitlabInstancePrivateToken gitlab)
        url = exportURL (_gitlabInstanceUrl gitlab) <> "/groups/" <> show project_id <> "/projects"
    r <- asJSON =<< getWith opts url
    return $ r ^? responseBody