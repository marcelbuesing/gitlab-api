{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}


module Main where

import Control.Monad.Trans.Class
import Data.Text.Internal.Lazy as TL
import Network.HTTP.Types.Status (ok200)
import Web.Scotty

import GitlabHooks.Data



main :: IO ()
main = scotty 3000 $
  post "/triggerci" $ do
    hdr <- header "X-Gitlab-Event"
    evt <- jsonData :: ActionM GitlabEvent
    lift $ print $ show evt
    status ok200

type GitlabHeader = TL.Text

