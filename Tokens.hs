{-# LANGUAGE OverloadedStrings #-}
module Tokens where

import System.Environment

import Data.ByteString.Char8
import Web.Twitter.Conduit
import Web.Authenticate.OAuth (OAuth(..), Credential(..))

-- | Put your key and secret in the env as TWITTER_OAUTH_CONSUMER_KEY
-- and TWITTER_OAUTH_CONSUMER_SECRET.
getTokens :: IO OAuth
getTokens = do
  key <- getEnv "TWITTER_OAUTH_CONSUMER_KEY"
  secret <- getEnv "TWITTER_OAUTH_CONSUMER_SECRET"
  return twitterOAuth
            { oauthConsumerKey = pack key
            , oauthConsumerSecret = pack secret
            }

-- | Put your token, token secret, user id, and scree name in the
-- environment with the correct keys.
getCreds = do
  token <- getEnv "TWITTER_OAUTH_TOKEN"
  token_secret <- getEnv "TWITTER_OAUTH_TOKEN_SECRET"
  user_id <- getEnv "TWITTER_USER_ID"
  screen_name <- getEnv "TWITTER_SCREEN_NAME"
  return Credential
             { unCredential = [("oauth_token", pack token),
                               ("oauth_token_secret", pack token_secret),
                               ("user_id", pack user_id),
                               ("screen_name", pack screen_name)]}
