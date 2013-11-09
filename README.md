createsend-haskell
==================

A Haskell wrapper for Campaign Monitor's HTTP API v.3.

Example use:

```haskell
{-# LANGUAGE OverloadedStrings #-}


import 		 CreateSendAPI.V3
import 		 Data.Aeson.Types	(Result (..))
import qualified Data.ByteString.Char8	as BSC8
import qualified Data.Foldable		as F

main :: IO ()
main = do
    putStr "Campaign Monitor API Key: "
    apiKey <- getLine
    putStr "List ID: "
    listId <- getLine
    let authReq = reqFromAPIKey (BSC8.pack apiKey)
    let listReq = listReqFromAPIKeyReq authReq (BSC8.pack listId)
    -- | Print the test list's web hooks:
    hooks <- getWebhooks listReq
    F.mapM_ (putStrLn . show) hooks
    return ()
```

You should be able to see you webhooks when you compile and run the above:

	Campaign Monitor API Key: yourAPIKeyHere
	List ID: yourListIDHere
	Webhook {webhookID = "9d8b90d389501ff9b87fb10489121ae4", webhookEvents = WebhookEvents {webhookEvtSubscribe = True, webhookEvtDeactivate = True, webhookEvtUpdate = True}, webhookURL = "http://63883c55.ngrok.com/CampaignMonitor/event", webhookStatus = "Active", webhookPayloadFormat = WebhookFmtJSON}

GPL license,
(C) Pavel M. Penev, 2013
