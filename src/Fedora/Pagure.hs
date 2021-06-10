{-# LANGUAGE CPP, OverloadedStrings #-}

{- |
Copyright: (c) 2020 Jens Petersen
SPDX-License-Identifier: GPL-2.0-only
Maintainer: Jens Petersen <petersen@redhat.com>

Pagure REST client library
-}

module Fedora.Pagure
  ( pagureProjectInfo
  , pagureListProjects
  , pagureListProjectIssues
  , IssueTitleStatus(..)
  , pagureListProjectIssueTitlesStatus
  , pagureProjectIssueInfo
  , pagureListGitBranches
  , pagureListGitBranchesWithCommits
  , pagureListUsers
  , pagureUserForks
  , pagureUserInfo
  , pagureUserRepos
  , pagureListGroups
  , pagureProjectGitURLs
  , queryPagure
  , queryPagureSingle
  , queryPagurePaged
  , queryPagureCount
  , makeKey
  , makeItem
  , maybeKey
  , Query
  , QueryItem
  , lookupKey
  , lookupKey'
  ) where

import Control.Monad
#if (defined(VERSION_lens_aeson))
import Control.Lens
import Data.Aeson.Lens
#else
import Lens.Micro
import Lens.Micro.Aeson
#endif
import Data.Aeson.Types
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Query
import System.IO (hPutStrLn, stderr)

-- | Project info
--
-- `pagureProjectInfo server "<repo>"`
-- `pagureProjectInfo server "<namespace>/<repo>"`
--
-- https://pagure.io/api/0/#projects-tab
pagureProjectInfo :: String -> String -> IO (Either String Value)
pagureProjectInfo server project = do
  let path = project
  queryPagureSingle server path []

-- | List projects
--
-- https://pagure.io/api/0/#projects-tab
pagureListProjects :: String -> Query -> IO Value
pagureListProjects server params = do
  let path = "projects"
  queryPagure server path params

-- | List project issues
--
-- https://pagure.io/api/0/#issues-tab
pagureListProjectIssues :: String -> String -> Query -> IO (Either String Value)
pagureListProjectIssues server repo params = do
  let path = repo +/+ "issues"
  queryPagureSingle server path params

data IssueTitleStatus =
  IssueTitleStatus { pagureIssueId :: Integer
                   , pagureIssueTitle :: String
                   , pagureIssueStatus :: T.Text
                   , pagureIssueCloseStatus :: Maybe T.Text
                   }

-- | List project issue titles
--
-- https://pagure.io/api/0/#issues-tab
pagureListProjectIssueTitlesStatus :: String -> String -> Query
  -> IO (Either String [IssueTitleStatus])
pagureListProjectIssueTitlesStatus server repo params = do
  let path = repo +/+ "issues"
  res <- queryPagureSingle server path params
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^.. key (T.pack "issues") . values . _Object & mapMaybe parseIssue
  where
    parseIssue :: Object -> Maybe IssueTitleStatus
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        mcloseStatus <- obj .:? "close_status"
        return $ IssueTitleStatus id' (T.unpack title) status mcloseStatus

-- | Issue information
--
-- https://pagure.io/api/0/#issues-tab
pagureProjectIssueInfo :: String -> String -> Int -> IO (Either String Object)
pagureProjectIssueInfo server repo issue = do
  let path = repo +/+ "issue" +/+ show issue
  res <- queryPagureSingle server path []
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^. _Object

-- | List repo branches
--
-- https://pagure.io/api/0/#projects-tab
pagureListGitBranches :: String -> String -> IO (Either String [String])
pagureListGitBranches server repo = do
  let path = repo +/+ "git/branches"
  res <- queryPagureSingle server path []
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^.. key (T.pack "branches") . values . _String & map T.unpack

-- | List repo branches with commits
--
-- https://pagure.io/api/0/#projects-tab
pagureListGitBranchesWithCommits :: String -> String -> IO (Either String Object)
pagureListGitBranchesWithCommits server repo = do
  let path = repo +/+ "git/branches"
      params = makeKey "with_commits" "1"
  res <- queryPagureSingle server path params
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^. key (T.pack "branches") . _Object

-- | List users
--
-- https://pagure.io/api/0/#users-tab
pagureListUsers :: String -> String -> IO Value
pagureListUsers server pat = do
  let path = "users"
      params = makeKey "pattern" pat
  queryPagure server path params

-- | User information
--
-- https://pagure.io/api/0/#users-tab
pagureUserInfo :: String -> String -> Query -> IO (Either String Value)
pagureUserInfo server user params = do
  let path = "user" +/+ user
  queryPagureSingle server path params

-- | List groups
--
-- https://pagure.io/api/0/#groups-tab
pagureListGroups :: String -> Maybe String -> Query -> IO Value
pagureListGroups server mpat paging = do
  let path = "groups"
      params = maybeKey "pattern" mpat ++ paging
  queryPagure server path params

-- | Project Git URLs
--
-- https://pagure.io/api/0/#projects-tab
pagureProjectGitURLs :: String -> String -> IO (Either String Value)
pagureProjectGitURLs server repo = do
  let path = repo +/+ "git/urls"
  queryPagureSingle server path []


-- | low-level query
queryPagure :: String -> String -> Query -> IO Value
queryPagure server path params =
  let url = "https://" ++ server +/+ "api/0" +/+ path
  in webAPIQuery url params

-- | single query
queryPagureSingle :: String -> String -> Query -> IO (Either String Value)
queryPagureSingle server path params = do
  res <- queryPagure server path params
  if isJust (res ^? key "error") then
    return $ Left (res ^. key "error" . _String & T.unpack)
    else
    return $ Right res

-- | count total number of hits
queryPagureCount :: String -> String -> Query -> String -> IO (Maybe Integer)
queryPagureCount server path params pagination = do
  res <- queryPagure server path (params ++ makeKey "per_page" "1")
  return $ res ^? key (T.pack pagination) . key "pages" . _Integer

-- | get all pages of results
--
-- Note this can potentially download very large amount of data.
-- For potentially large queries, it is a good idea to queryPagureCount first.
queryPagurePaged :: String -> String -> Query -> (String,String) -> IO [Value]
queryPagurePaged server path params (pagination,paging) = do
  -- FIXME allow overriding per_page
  let maxPerPage = "100"
  res1 <- queryPagure server path (params ++ makeKey "per_page" maxPerPage)
  let mpages = res1 ^? key (T.pack pagination) . key "pages" . _Integer
  case mpages of
    Nothing -> return []
    Just pages -> do
      when (pages > 1) $
        hPutStrLn stderr $ "receiving " ++ show pages ++ " pages × " ++ maxPerPage ++ " results..."
      rest <- mapM nextPage [2..pages]
      return $ res1 : rest
  where
    nextPage p =
      queryPagure server path (params ++ makeKey "per_page" "100" ++ makeKey paging (show p))

-- | list user's repos
pagureUserRepos :: String -> String -> IO [Text]
pagureUserRepos server user = do
  let path = "user" +/+ user
  pages <- queryPagurePaged server path [] ("repos_pagination", "repopage")
  return $ concatMap (getRepos "repos") pages

getRepos :: Text -> Value -> [Text]
getRepos field result =
  result ^.. key field . values . key "fullname" . _String

-- | list user's forks
pagureUserForks :: String -> String -> IO [Text]
pagureUserForks server user = do
  let path = "user" +/+ user
  pages <- queryPagurePaged server path [] ("forks_pagination", "forkpage")
  return $ concatMap (getRepos "forks") pages
