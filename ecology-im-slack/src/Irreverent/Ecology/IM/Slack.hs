{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Irreverent.Ecology.IM.Slack (
  -- Types
    SlackIMError(..)
  , SlackSyncNotificationLine(..)
  , SlackSyncNotificationJson(..)
  -- Functions
  , renderSlackIMError
  , slackAPI
  , syncNotification
  ) where

import Irreverent.Ecology.Core (GitRepository(..))
import Irreverent.Ecology.API.IM (IMAPI(..))

import Ultra.Control.Lens ((.~), (&))
import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT)
import Ultra.Data.Aeson (ToJSON(..), Value, (.=), object)
import qualified Ultra.Data.Text as T
import qualified Ultra.Network.HTTP.Client as H

import qualified Network.Wreq as W
import qualified Network.Wreq.Session as S

import qualified Data.ByteString.Lazy as BSL

import Preamble

newtype SlackIMError =
  SlackIMHTTPError (H.HttpClientError Value BSL.ByteString)
  deriving (Show)

renderSlackIMError
  :: SlackIMError
  -> T.Text
renderSlackIMError (SlackIMHTTPError e) = T.pack . show $ e

data SlackSyncNotificationLine =
  SlackSyncNotificationLine {
    ssnlName  :: !T.Text
  , ssnlUrl   :: !T.Text
  } deriving (Show, Eq)

lineToAttachment :: SlackSyncNotificationLine -> Value
lineToAttachment (SlackSyncNotificationLine n gurl) = object [
    "fallback" .= T.concat ["Name: ", n, ", Git URL: ", gurl]
  , "color" .= t "#9c62ff"
  , "fields" .= [
      object [
          "title" .= t "Name"
        , "value" .= n
        , "short" .= True
        ],
      object [
          "title" .= t "Git URL"
        , "value" .= gurl
        , "short" .= True
        ]
    ]
  ]

newtype SlackSyncNotificationJson =
  SlackSyncNotificationJson {
    ssnjNewProjects :: NonEmpty SlackSyncNotificationLine
  } deriving (Show, Eq)

instance ToJSON SlackSyncNotificationJson where
--toJSON :: a -> Value
  toJSON (SlackSyncNotificationJson newProjects) = object [
      "text"        .= t "Introducing some new projects!"
    , "attachments" .= (lineToAttachment <$> newProjects)
    ]

-- END

slackAPI :: (MonadCatch m, MonadIO m) => T.Text -> IMAPI m SlackIMError
slackAPI webhook = IMAPI
  (syncNotification webhook)

syncNotification
  :: (MonadCatch m, MonadIO m)
  => T.Text
  -> [GitRepository]
  -> EitherT SlackIMError m ()
syncNotification webhook repos =
  let
    msg' :: Maybe Value
    msg' = fmap (toJSON . SlackSyncNotificationJson)
      . nonEmpty
      . fmap syncLine
      $ repos

    opts :: W.Options
    opts = W.defaults
      & (W.redirects     .~ 10)
      . (W.checkResponse .~ Nothing)
  in for_ msg' $ \msg -> do
    s <- liftIO S.newSession
    firstEitherT SlackIMHTTPError . void . H.checkResponseStatus . liftIO $ S.postWith opts s (T.unpack webhook) msg

-- TODO: Really want the project info here
syncLine :: GitRepository -> SlackSyncNotificationLine
syncLine repo = SlackSyncNotificationLine
  (gitRepoName repo)
  (gitRepoURL repo) -- TODO: Should maybe add the web location for the repo, as this will be the git url.

t :: T.Text -> T.Text
t = id
