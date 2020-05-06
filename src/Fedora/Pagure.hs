{-# LANGUAGE CPP #-}

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
  , pagureListProjectIssueTitles
  , pagureProjectIssueInfo
  , pagureListGitBranches
  , pagureListGitBranchesWithCommits
  , pagureListUsers
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
--import Network.HTTP.Conduit (queryString)
import Data.Aeson.Types
#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,3))
#else
import Data.ByteString (ByteString)
#endif
import qualified Data.ByteString.Char8 as B
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Simple
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,1))
#else
type Query = [(ByteString, Maybe ByteString)]
#endif
#if (defined(MIN_VERSION_http_conduit) && MIN_VERSION_http_conduit(2,3,3))
#else
type QueryItem = (ByteString, Maybe ByteString)
#endif

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
  let path = repo </> "issues"
  queryPagureSingle server path params

-- | List project issue titles
--
-- https://pagure.io/api/0/#issues-tab
pagureListProjectIssueTitles :: String -> String -> Query
                             -> IO (Either String [(Integer,String,T.Text)])
pagureListProjectIssueTitles server repo params = do
  let path = repo </> "issues"
  res <- queryPagureSingle server path params
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^.. key (T.pack "issues") . values . _Object & mapMaybe parseIssue
  where
    parseIssue :: Object -> Maybe (Integer, String, Text)
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',T.unpack title,status)

-- | Issue information
--
-- https://pagure.io/api/0/#issues-tab
pagureProjectIssueInfo :: String -> String -> Int -> IO (Either String Object)
pagureProjectIssueInfo server repo issue = do
  let path = repo </> "issue" </> show issue
  res <- queryPagureSingle server path []
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^. _Object

-- | List repo branches
--
-- https://pagure.io/api/0/#projects-tab
pagureListGitBranches :: String -> String -> IO (Either String [String])
pagureListGitBranches server repo = do
  let path = repo </> "git/branches"
  res <- queryPagureSingle server path []
  return $ case res of
    Left e -> Left e
    Right v -> Right $ v ^.. key (T.pack "branches") . values . _String & map T.unpack

-- | List repo branches with commits
--
-- https://pagure.io/api/0/#projects-tab
pagureListGitBranchesWithCommits :: String -> String -> IO (Either String Object)
pagureListGitBranchesWithCommits server repo = do
  let path = repo </> "git/branches"
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
  let path = "user" </> user
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
  let path = repo </> "git/urls"
  queryPagureSingle server path []


-- | low-level query
queryPagure :: String -> String -> Query -> IO Value
queryPagure server path params = do
  let url = "https://" ++ server </> "api/0" </> path
  req <- setRequestQueryString params <$> parseRequest url
  -- putStrLn $ url ++ B.unpack (queryString req)
  getResponseBody <$> httpJSON req

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

-- | Maybe create a query key
maybeKey :: String -> Maybe String -> Query
maybeKey _ Nothing = []
maybeKey k mval = [(B.pack k, fmap B.pack mval)]

-- | make a singleton key-value Query
makeKey :: String -> String -> Query
makeKey k val = [(B.pack k, Just (B.pack val))]

-- | make a key-value QueryItem
makeItem :: String -> String -> QueryItem
makeItem k val = (B.pack k, Just (B.pack val))

-- | looks up key in object
lookupKey :: FromJSON a => Text -> Object -> Maybe a
lookupKey k = parseMaybe (.: k)

-- -- | looks up Text from key in object
-- lookupText :: Text -> Object -> Maybe Text
-- lookupText k = parseMaybe (.: k)

-- | like lookupKey but raises an error if no key found
lookupKey' :: FromJSON a => Text -> Object -> a
lookupKey' k obj =
  fromMaybe (error ("no key: " ++ show k)) (lookupKey k obj)

-- | list user's repos
pagureUserRepos :: String -> String -> IO [Text]
pagureUserRepos server user = do
  let path = "user" </> user
  pages <- queryPagurePaged server path [] ("repos_pagination", "repopage")
  return $ concat $ map getRepos pages
  where
    getRepos :: Value -> [Text]
    getRepos result =
      let repos = result ^.. key (T.pack "repos") . values . _Object
        in map getRepo repos

    getRepo :: Object -> T.Text
    getRepo repo =
      case parseRepo repo of
        Nothing -> error "parsing repo failed"
        Just (mnamespace,name) ->
          case mnamespace of
            Nothing -> name
            Just nm -> T.concat [nm, T.singleton '/', name]

    parseRepo :: Object -> Maybe (Maybe Text, Text)
    parseRepo =
      parseMaybe $ \obj -> do
        namespace <- obj .:? "namespace"
        name <- obj .: "name"
        return (namespace,name)
