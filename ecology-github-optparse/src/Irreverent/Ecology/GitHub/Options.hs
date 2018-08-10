{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Ecology.GitHub.Options
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Ecology.GitHub.Options (
  -- * Parsers
    githubAuthP
  ) where

import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T
import Ultra.Options.Applicative (
    Parser
  , eitherTextReader
  , envvar
  , long
  , option
  )

import GitHub.Auth (Auth(..))

import Preamble

authReader :: T.Text -> Either T.Text Auth
authReader t = case T.splitOn ":" t of
  [] -> Left t
  username:password -> pure . (BasicAuth `on` T.encodeUtf8) username $ T.intercalate ":" password

githubAuthP :: [(T.Text, T.Text)] -> Parser Auth
githubAuthP env = option (eitherTextReader authReader) $
     long "github-auth"
  <> envvar (either (const Nothing) pure . authReader) env "GITHUB_TOKEN" "GitHub Login information, in the form 'username:token'"
