{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Kevlar.GitHub.Status where

import qualified Data.ByteString.Char8 as ByteString
import Data.Maybe (maybe)
import qualified Data.Text as Text
import qualified GitHub
import qualified Kevlar.Git as Git
import Kevlar.GitHub.ParseUrl (parseUrl)
import qualified Kevlar.Status as Status
import System.Environment (lookupEnv)

-- | Set the status of repository on GitHub
status :: Git.Repository -> Status.Status -> IO ()
status (Git.WorkingCopy _) _status = return ()
status (Git.Url url commit) status = do
  token <- lookupEnv "GITHUB_AUTH_TOKEN"
  case token of
    Nothing -> print "GITHUB_AUTH_TOKEN is not set, skipping GitHub status"
    Just auth ->
      case parseUrl url of
        Nothing -> print $ "Unable to parse the GitHub owner and repository name" <> url
        Just (owner, repo) -> postStatus (GitHub.OAuth $ ByteString.pack auth) owner repo
  where
    postStatus auth owner repo = do
      let params =
            GitHub.NewStatus
              (toGitHubStatus status)
              (Just $ GitHub.URL "https://github.com/wagdav/kevlar")
              (Just "build")
              (Just "Kevlar CI")
      let req =
            GitHub.createStatusR
              (GitHub.mkOwnerName $ Text.pack owner)
              (GitHub.mkRepoName $ Text.pack repo)
              (GitHub.mkCommitName $ Text.pack commit)
              params
      res <- GitHub.github auth req
      case res of
        Right _ -> return ()
        Left err -> print err
    toGitHubStatus = \case
      Status.Error -> GitHub.StatusError
      Status.Failure -> GitHub.StatusFailure
      Status.Pending -> GitHub.StatusPending
      Status.Success -> GitHub.StatusSuccess
