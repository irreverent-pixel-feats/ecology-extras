{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.Git.GitHub
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.Git.GitHub (
  -- * Types
    GitHubEcologyError(..)
  -- * Functions
  , createNewGHRepo
  , renderGitHubEcologyError
  , getGHOrgRepos
  , githubAPI
  ) where

import Irreverent.Ecology.API.Git (GitPlatformAPI(..))
import Irreverent.Ecology.Core.Data (
    EcologyPrivacy(..)
  , EcologyProjectDescription(..)
  , EcologyProjectName(..)
  , GitRepository(..)
  , NewGitRepository(..)
  )

import Ultra.Control.Monad.Trans.Either (EitherT, pattern EitherT, firstEitherT)
import qualified Ultra.Data.Text as T

import qualified GitHub.Data.Definitions as G
import qualified GitHub.Data.Name as G
import qualified GitHub.Data.Repos as G
import qualified GitHub.Endpoints.Repos as G

import Preamble

newtype GitHubEcologyError =
  GitHubAPIError G.Error
  deriving (Show)


renderGitHubEcologyError :: GitHubEcologyError -> T.Text
renderGitHubEcologyError (GitHubAPIError e) = T.concat ["Github API Error: ", T.pack . show $ e]

githubAPI
  :: (MonadIO m)
  => G.Auth
  -> Maybe T.Text
  -> GitPlatformAPI a b m GitHubEcologyError
githubAPI auth org = GitPlatformAPI
  (getGHOrgRepos auth org)
  (createNewGHRepo auth org)

liftGH
  :: (MonadIO m)
  => IO (Either G.Error a)
  -> EitherT GitHubEcologyError m a
liftGH = firstEitherT GitHubAPIError . EitherT . liftIO

convertGitHubRepo :: G.Repo -> GitRepository
convertGitHubRepo = GitRepository
  <$> G.untagName . G.repoName
  <*> G.getUrl . G.repoUrl

toNewGHRepo
  :: NewGitRepository a b
  -> G.NewRepo
toNewGHRepo =
  let
    privacy' :: EcologyPrivacy -> Bool
    privacy' EcologyPrivate = True
    privacy' EcologyPublic = False
  in G.NewRepo
    <$> G.N . ecologyProjectNameText . newRepoName
    <*> pure . ecologyProjectDescriptionText . newRepoDescription
    <*> pure Nothing
    <*> pure . privacy' . newRepoPrivacy
    <*> pure (pure False)
    <*> pure (pure False)
    <*> pure (pure False)

getGHOrgRepos
  :: (MonadIO m)
  => G.Auth
  -> Maybe T.Text
  -> EitherT GitHubEcologyError m [GitRepository]
getGHOrgRepos auth mOrg =
  let
    listRepos = maybe
      (G.currentUserRepos auth G.RepoPublicityAll)
      (\org -> G.organizationRepos' (pure auth) (G.N org) G.RepoPublicityAll)
      mOrg
  in fmap convertGitHubRepo . toList
    <$> liftGH listRepos

createNewGHRepo
  :: (MonadIO m)
  => G.Auth
  -> Maybe T.Text -- Optional organisation
  -> NewGitRepository a b
  -> EitherT GitHubEcologyError m GitRepository
createNewGHRepo auth mOrg ngr =
  let
    nr :: G.NewRepo
    nr = toNewGHRepo ngr
  in do
    gnr <- liftGH $ maybe
      (G.createRepo' auth nr)
      (\org -> G.createOrganizationRepo' auth (G.N org) nr)
      mOrg
    pure (convertGitHubRepo gnr)
