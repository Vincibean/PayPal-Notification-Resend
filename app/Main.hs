{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson.Lens      (key, values, _String)
import           Data.ByteString.Lazy (ByteString)
import           Data.Map             as Map
import           Data.Text            (Text, unpack)
import           Data.Text.Encoding   (encodeUtf8)
import           GHC.Generics
import           Lib
import           Network.Wreq

data Webhooks = Webhooks { webhook_ids :: [String] } deriving (Show,Eq,Generic)

instance ToJSON Webhooks

clientCredentials :: String
clientCredentials = "client_credentials"

main :: IO ()
main = do
    let accessTokenOpts = defaults & auth ?~ basicAuth "clientID" "clientSecret"
    let accessTokenParams = ["grant_type" := clientCredentials]
    accessTokenResp <- postWith accessTokenOpts "https://api.paypal.com/v1/oauth2/token" accessTokenParams
    let accessToken = accessTokenResp ^. responseBody . key "access_token" . _String

    let webhookEventsOpts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken) & header "Content-Type" .~ ["application/json"] & param "start_time" .~ ["2020-02-27T09:00:00Z"] & param "end_time" .~ ["2020-02-27T11:00:00Z"]
    webhookEventsResp <- getWith webhookEventsOpts "https://api.paypal.com/v1/notifications/webhooks-events"
    let webhookEventIds = webhookEventsResp ^.. responseBody . key "events" . values . key "id" . _String

    resp <- traverse (resend accessToken . unpack) webhookEventIds

    let webhookEventResps = fmap (^. responseBody . key "id" . _String) resp

    putStrLn $ show webhookEventResps


resend :: Text -> String -> IO (Response ByteString)
resend accessToken webhookEventId = do
    let webhookEventsOpts = defaults & auth ?~ oauth2Bearer (encodeUtf8 accessToken) & header "Content-Type" .~ ["application/json"]
    let webhookEventsParams = toJSON $ Webhooks ["webhook-id"]
    postWith webhookEventsOpts ("https://api.paypal.com/v1/notifications/webhooks-events/" ++ webhookEventId ++ "/resend") webhookEventsParams
