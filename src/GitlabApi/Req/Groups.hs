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

listGroupProjects :: GitlabInstance -> GroupsId -> Int -> IO (Maybe [ListGroupProject])
listGroupProjects gitlab groups_id count = do
    let opts = privateTokenHeader (_gitlabInstancePrivateToken gitlab)
        url = exportURL (_gitlabInstanceUrl gitlab) <> "/groups/" <> show groups_id <> "/projects?per_page=" <> show count
    r <- asJSON =<< getWith opts url
    return $ r ^? responseBody