{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.CI.BitbucketPipelines
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.CI.BitbucketPipelines (
  -- * Types
    CIPipelinesError(..)
  , BitbucketPipelinesError(..)
  -- * Functions
  , bitbCIEnvVarUpdate
  , bitbucketPipelinesAPI
  , configBitbPipelines
  , configBitbPipelinesRepo
  , renderBitbucketPipelinesError
  , renderCIPipelinesError
  ) where

import Irreverent.Ecology.Core (
    EcologyHashMap(..)
  , EcologyParameters(..)
  , EcologyProjectName(..)
  , EcologyEnvironmentPair(..)
  , EcologyEnvironmentVariableUpdate(..)
  , EnvironmentVariableName(..)
  , EnvironmentVariableValue(..)
  , NewCIInfo(..)
  )
import Irreverent.Ecology.API.CI (
    CIAPI(..)
  )

import qualified Irreverent.Bitbucket.Core as B (Username, RepoName(..))
import qualified Irreverent.Bitbucket.Core.Control as B (BitbucketT(..), runBitbucketT)
import qualified Irreverent.Bitbucket.Core.Data.Auth as B (Auth(..))
import qualified Irreverent.Bitbucket.Core.Data.Common as B
import Irreverent.Bitbucket.Core.Data.Pipelines.EnvironmentVariable (PipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewSSHKeyPair (NewPipelinesSSHKeyPair(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.NewEnvironmentVariable (NewPipelinesEnvironmentVariable(..))
import Irreverent.Bitbucket.Core.Data.Pipelines.UpdateConfig (UpdatePipelinesConfig(..))
import Irreverent.Bitbucket.Http.Repositories.Pipelines.AddSSHKeyPair (addSSHKeyPair)
import Irreverent.Bitbucket.Http.Repositories.Pipelines.AddEnvironmentVariable (addPipelinesEnvironmentVariable)
import Irreverent.Bitbucket.Http.Repositories.Pipelines.DeleteEnvironmentVariable (deletePipelinesEnvironmentVariable)
import Irreverent.Bitbucket.Http.Repositories.Pipelines.GetEnvironmentVariables (getPipelineEnvironmentVariables)
import Irreverent.Bitbucket.Http.Repositories.Pipelines.UpdateConfig (updatePipelinesConfig)
import qualified Irreverent.Bitbucket.Http.Error as B

import Ultra.Control.Monad.Bracket (MonadBracket)
import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, left, mapEitherT, unifyEitherT)
import qualified Ultra.Data.HashMap.Strict as H
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import Ultra.System.Verified.IO (
    NonExistenceFail(..)
  , NewFileOpenError(..)
  , doesNotExist
  , renderNewFileOpenError
  , renderNonExistenceFail
  , withNewFileForWriting
  )

import Crypto.Hash (Digest, SHA3_512, hash)

import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Validation (Validation(..))

import Network.Wreq.Session as S

import NeatInterpolation (text)

import Preamble

data CIPipelinesError =
    CIPipelinesFileNonExistenceError !NonExistenceFail
  | CIPipelinesFileOpenError !NewFileOpenError
  | CIPipelinesAPIError !BitbucketPipelinesError
    deriving (Show)

data BitbucketPipelinesError =
    BitbucketPipelinesAPIError !B.BitbucketAPIError
  | BitbucketPipelinesMissingParameters !(NonEmpty T.Text)
    deriving (Show)

renderCIPipelinesError :: CIPipelinesError -> T.Text
renderCIPipelinesError (CIPipelinesAPIError e) = renderBitbucketPipelinesError e
renderCIPipelinesError (CIPipelinesFileNonExistenceError e) = renderNonExistenceFail e
renderCIPipelinesError (CIPipelinesFileOpenError e) = renderNewFileOpenError e

renderBitbucketPipelinesError :: BitbucketPipelinesError -> T.Text
renderBitbucketPipelinesError (BitbucketPipelinesAPIError e) = B.renderBitbucketAPIError e
renderBitbucketPipelinesError (BitbucketPipelinesMissingParameters ns) =
  T.bracketedList "Could find the following parameters: [" "]" "," . toList $ ns

bitbucketPipelinesAPI
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => (a -> T.Text) -- ^ Produce the bitbucket yaml file based on the project type.
  -> (a -> Bool) -- ^ Does the project type require access to other git repos?
  -> B.Auth
  -> B.Username
  -> B.PublicSSHKey
  -> B.PrivateSSHKey
  -> CIAPI a m CIPipelinesError
bitbucketPipelinesAPI yaml accessKeys auth owner pub priv =
  CIAPI
    (configBitbPipelinesRepo yaml)
    (\params info -> firstEitherT CIPipelinesAPIError $ configBitbPipelines auth owner pub priv accessKeys params info)
    (bitbCIEnvVarsUpdate auth owner)

bitbCIEnvVarsUpdate
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => B.Auth
  -> B.Username
  -> EcologyProjectName
  -> NonEmpty EcologyEnvironmentVariableUpdate
  -> EitherT CIPipelinesError m ()
bitbCIEnvVarsUpdate auth owner project changes =
  let
    repo :: B.RepoName
    repo = B.RepoName . ecologyProjectNameText $ project
  in firstEitherT CIPipelinesAPIError . mapEitherT (flip runReaderT auth . B.runBitbucketT) $ do
    session <- liftIO S.newSession
    vars <- bitb $ getPipelineEnvironmentVariables session owner repo
    traverse_ (bitbCIEnvVarUpdate session owner repo vars) changes

bitbCIEnvVarUpdate
  :: forall m. (MonadBracket m, MonadCatch m, MonadIO m)
  => S.Session
  -> B.Username
  -> B.RepoName
  -> [PipelinesEnvironmentVariable]
  -> EcologyEnvironmentVariableUpdate
  -> EitherT BitbucketPipelinesError (B.BitbucketT m) ()
bitbCIEnvVarUpdate session owner repo vars =
  let
    existingVar
      :: T.Text
      -> Maybe B.Uuid
    existingVar var = L.lookup var . fmap ((,) <$> pevKey <*> pevUuid) $ vars

    deleteVar
      :: T.Text
      -> EitherT BitbucketPipelinesError (B.BitbucketT m) ()
    deleteVar varName =
      for_ (existingVar varName) $
        bitb . deletePipelinesEnvironmentVariable session owner repo

    newVar
      :: T.Text
      -> T.Text
      -> EitherT BitbucketPipelinesError (B.BitbucketT m) ()
    newVar varName val = void $ do
      deleteVar varName
      bitb . addPipelinesEnvironmentVariable session owner repo $
        NewPipelinesEnvironmentVariable varName val B.SecuredVariable

  in \case
    EcologyEnvironmentVariableUpdate varName _ _ newVal -> newVar (envVarName varName) newVal
    EcologyEnvironmentVariableNew varName _ val -> newVar (envVarName varName) val
    EcologyEnvironmentVariableDelete varName -> void . deleteVar . envVarName $ varName

configBitbPipelinesRepo
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => (a -> T.Text)
  -> T.Text
  -> EcologyParameters
  -> NewCIInfo a
  -> EitherT CIPipelinesError m (Maybe T.Text)
configBitbPipelinesRepo yaml path _ ciInfo =
  unifyEitherT CIPipelinesFileNonExistenceError $
    doesNotExist (path <> "/bitbucket-pipelines.yml") $ \ufp ->
      fmap fst . unifyEitherT CIPipelinesFileOpenError . withNewFileForWriting ufp $ \h -> do
        liftIO $ BS.hPut h (T.encodeUtf8 . yaml . nciProjectType $ ciInfo)
        pure . pure $ [text|
          bitbucket-pipelines.yml

          - Configuring CI
        |]

bitb
  :: (Functor f)
  => EitherT B.BitbucketAPIError f a
  -> EitherT BitbucketPipelinesError f a
bitb = firstEitherT BitbucketPipelinesAPIError

configBitbPipelines
  :: (MonadBracket m, MonadCatch m, MonadIO m)
  => B.Auth
  -> B.Username
  -> B.PublicSSHKey
  -> B.PrivateSSHKey
  -> (a -> Bool) -- ^ Does the project type require Access Keys?
  -> EcologyParameters
  -> NewCIInfo a
  -> EitherT BitbucketPipelinesError m EcologyHashMap
configBitbPipelines auth owner pub priv accessKeys params newCI =
  let
    repoName :: B.RepoName
    repoName = B.RepoName
      . ecologyProjectNameText
      . nciProjectName
      $ newCI

    hash' :: T.Text -> Digest SHA3_512
    hash' = hash . T.encodeUtf8

  in mapEitherT (flip runReaderT auth . B.runBitbucketT) $ do
    session <- liftIO S.newSession
    void . bitb $ updatePipelinesConfig session owner repoName (UpdatePipelinesConfig True)
    when (accessKeys . nciProjectType $ newCI) $ do
      void . bitb $ addSSHKeyPair session owner repoName (NewPipelinesSSHKeyPair priv pub)
    mEnv <- pure . for (nciEnvironment newCI) $ \case
      EcologyEnvironmentPair (EnvironmentVariableName varName) ref ->
          either (Failure . pure) (pure . (,) varName) $ getEnvironmentValue ref params
    case mEnv of
      Failure missing -> left $ BitbucketPipelinesMissingParameters missing
      Success env -> forM_ env $ \(name, val) ->
        bitb . addPipelinesEnvironmentVariable session owner repoName $
          NewPipelinesEnvironmentVariable name val B.SecuredVariable
    pure . EcologyHashMap . H.fromList $ [
        ("private", T.pack . show . hash' . B.privateKeyText $ priv)
      , ("public", T.pack . show . hash' . B.publicKeyText $ pub)
      ]

-- TODO: Put these back in ecology
getEnvironmentValue :: EnvironmentVariableValue -> EcologyParameters -> Either T.Text T.Text
getEnvironmentValue (EnvironmentVariableValue x) _ = pure x
getEnvironmentValue (EnvironmentVariableReferenceValue ref) (EcologyParameters ps) = maybe (Left ref) pure $ H.lookup ref ps
