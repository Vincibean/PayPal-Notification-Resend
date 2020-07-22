{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-cse #-}

module Main where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens (key, values, _String)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, toStrict)
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text, unpack)
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Lib
import Network.Wreq
import System.Console.CmdArgs
  ( Data,
    Typeable,
    cmdArgs,
    def,
    help,
    program,
    summary,
    typ,
    (&=),
  )

newtype Webhooks = Webhooks {webhook_ids :: [String]} deriving (Show, Eq, Generic)

instance ToJSON Webhooks

clientCredentials :: String
clientCredentials = "client_credentials"

data PayPal = PayPal
  { client_id :: Maybe String,
    client_secret :: Maybe String,
    webhook_id :: Maybe String
  }
  deriving (Show, Data, Typeable)

paypal =
  PayPal
    { client_id = def &= typ "STRING" &= help "PayPal Client ID",
      client_secret = def &= typ "STRING" &= help "PayPal Client Secret",
      webhook_id = def &= typ "STRING" &= help "PayPal WebHook ID"
    }
    &= program "PayPal Notification Resend"
    &= summary "Resend past PayPal notifications"

main :: IO ()
main = do
  paypalArgs <- cmdArgs paypal
  let clientId = LBS.toStrict . pack . fromMaybe (error "No Client ID defined") . client_id $ paypalArgs
  let clientSecret = LBS.toStrict . pack . fromMaybe (error "No Client Secret defined") . client_secret $ paypalArgs
  let webhookId = fromMaybe (error "No WebHook ID defined") . webhook_id $ paypalArgs

  accessToken <- accessToken clientId clientSecret

  (webhookEventIds, next) <- webhookEventIds accessToken

  resp <- traverse (resend webhookId accessToken . unpack) webhookEventIds
  let webhookEventResps = fmap (^. responseBody . key "id" . _String) resp

  print webhookEventResps

resend :: String -> Text -> String -> IO (Response LBS.ByteString)
resend webhookId accessToken webhookEventId = do
  let webhookEventsOpts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken) & header "Content-Type" .~ ["application/json"]
  let webhookEventsParams = toJSON $ Webhooks [webhookId]
  postWith webhookEventsOpts ("https://api.paypal.com/v1/notifications/webhooks-events/" ++ webhookEventId ++ "/resend") webhookEventsParams

accessToken :: ByteString -> ByteString -> IO Text
accessToken clientId clientSecret = do
  let accessTokenOpts = defaults & auth ?~ basicAuth clientId clientSecret
  let accessTokenParams = ["grant_type" := clientCredentials]
  accessTokenResp <- postWith accessTokenOpts "https://api.paypal.com/v1/oauth2/token" accessTokenParams
  let accessToken = accessTokenResp ^. responseBody . key "access_token" . _String
  return accessToken

webhookEventIds :: Text -> IO ([Text], Maybe Text)
webhookEventIds accessToken = do
  let webhookEventsOpts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken) & header "Content-Type" .~ ["application/json"] & param "start_time" .~ ["2020-02-27T09:00:00Z"] & param "end_time" .~ ["2020-02-27T11:00:00Z"] & param "page_size" .~ ["100"]
  webhookEventsResp <- getWith webhookEventsOpts "https://api.paypal.com/v1/notifications/webhooks-events"
  let webhookEventIds = webhookEventsResp ^.. responseBody . key "events" . values . key "id" . _String
  let next = webhookEventsResp ^? responseBody . key "links" . values . filtered (has (key "rel" . _String . only "next")) . key "href" . _String
  return (webhookEventIds, next)
