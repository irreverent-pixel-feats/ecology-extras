{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.Git.Bitbucket
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.Git.Bitbucket (
  -- * Types
    GitEcologyError(..)
  -- * Functions
  , bitbucketAPI
  , convertBitbRepo
  , createNewRepoBitbucket
  , getOrgReposBitbucket
  , renderGitEcologyError
  , toNewBitbRepo
  ) where

import Irreverent.Ecology.Core.Data (
    EcologyPrivacy(..)
  , EcologyProjectDescription(..)
  , GitRepository(..)
  , NewGitRepository(..)
  , ecologyProjectNameText
  )

import Irreverent.Ecology.API.Git (
    GitPlatformAPI(..)
  )

import qualified Irreverent.Bitbucket.Core as B (Username, RepoName(..))
import qualified Irreverent.Bitbucket.Core.Control as B (BitbucketT(..))
import qualified Irreverent.Bitbucket.Core.Data.Auth as B (Auth(..))
import qualified Irreverent.Bitbucket.Core.Data.Common as B
import qualified Irreverent.Bitbucket.Core.Data.Repository as B (Repository(..))
import qualified Irreverent.Bitbucket.Core.Data.NewRepository as B
import qualified Irreverent.Bitbucket.Http.Error as B
import qualified Irreverent.Bitbucket.Http.Repositories.List as B
import qualified Irreverent.Bitbucket.Http.Repositories.New as B

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

newtype GitEcologyError =
  BitbucketAPIError B.BitbucketAPIError
  deriving (Show)

renderGitEcologyError :: GitEcologyError -> T.Text
renderGitEcologyError (BitbucketAPIError e) = B.renderBitbucketAPIError e

-- |
-- A default implementation of a bitbucket backed git platform API
-- It creates the repo, assigning the project key
-- as the provided function directs.
-- It does not modify the access privileges of the
-- created repository
--
bitbucketAPI
  :: (MonadCatch m, MonadIO m)
  => (NewGitRepository a b -> Maybe B.ProjectKey)
  -> (a -> T.Text)
  -> B.Auth
  -> B.Username
  -> GitPlatformAPI a b m GitEcologyError
bitbucketAPI projectFunction renderType auth org = GitPlatformAPI
  (getOrgReposBitbucket auth org)
  (createNewRepoBitbucket projectFunction renderType auth org)

convertBitbRepo :: B.Repository -> GitRepository
convertBitbRepo repo =
  GitRepository
    (B.getRepoName . B.repoName $ repo)
    (B.getUri . B.hrefUrl . B.repoCloneSsh $ repo)
    (B.getUri . B.hrefUrl . B.repoHtml $ repo)

getOrgReposBitbucket
  :: (MonadCatch m, MonadIO m)
  => B.Auth
  -> B.Username
  -> EitherT GitEcologyError m [GitRepository]
getOrgReposBitbucket auth org =
  firstEitherT BitbucketAPIError . mapEitherT (flip runReaderT auth . B.runBitbucketT)$ do
      session <- liftIO S.newSession
      bitbRepos <- B.listRepositories session org
      pure $ fmap convertBitbRepo bitbRepos

toNewBitbRepo
  :: (NewGitRepository a b -> Maybe B.ProjectKey)
  -> (a -> T.Text)
  -> NewGitRepository a b
  -> B.NewRepository
toNewBitbRepo projectFunction renderType =
  let
    privacy' :: EcologyPrivacy -> B.Privacy
    privacy' EcologyPrivate = B.Private
    privacy' EcologyPublic = B.Public

    forkPolicy' :: EcologyPrivacy -> B.ForkPolicy
    forkPolicy' EcologyPrivate = B.NoPublicForksPolicy
    forkPolicy' EcologyPublic = B.ForkAwayPolicy
  in B.NewRepository
    <$> B.RepoDescription . ecologyProjectDescriptionText . newRepoDescription
    <*> pure B.Git
    <*> projectFunction
    <*> forkPolicy' . newRepoPrivacy
    <*> privacy' . newRepoPrivacy
    <*> B.Language . renderType . newRepoType
    <*> pure B.NoWiki
    <*> pure B.HasIssues

createNewRepoBitbucket
  :: (MonadCatch m, MonadIO m)
  => (NewGitRepository a b -> Maybe B.ProjectKey)
  -> (a -> T.Text)
  -> B.Auth
  -> B.Username
  -> NewGitRepository a b
  -> EitherT GitEcologyError m GitRepository
createNewRepoBitbucket projectFunction renderType auth owner newRepo =
  firstEitherT BitbucketAPIError . mapEitherT (flip runReaderT auth . B.runBitbucketT) $ do
    session <- liftIO S.newSession
    convertBitbRepo <$>
      B.createRepo
        session
        owner
        (B.RepoName . ecologyProjectNameText . newRepoName $ newRepo)
        (toNewBitbRepo projectFunction renderType newRepo)
