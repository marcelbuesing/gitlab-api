{-# LANGUAGE OverloadedStrings #-}

module GitlabApi.Req.Common where

import Network.Wreq
import Control.Lens
import Data.Text.Encoding(encodeUtf8)
import GHC.Generics
import GitlabApi.Data.ApiTypes

privateTokenHeader :: PrivateToken -> Options
privateTokenHeader privateToken = defaults & header "Private-Token" .~ [encodeUtf8 (_unToken privateToken)]