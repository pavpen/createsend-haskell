{-# LANGUAGE	  DoAndIfThenElse
 		, FlexibleContexts
		, OverloadedStrings
		, RecordWildCards #-}

module CreateSendAPI.V3.List.Webhooks where

import           Control.Applicative	 ((<$>), (<*>))
import           Control.Monad.IO.Class	 (MonadIO)
import           Data.Aeson		 (FromJSON (parseJSON), Value (Object),
					  (.:), (.=), encode, fromJSON, object)
import           Data.Aeson.Parser       (json)
import           Data.Aeson.Types	 (Result (..))
import qualified Data.ByteString	 as BS
import qualified Data.ByteString.Char8	 as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy.Internal as LBS
import           Data.Conduit            (($$+-))
import qualified Data.Conduit		 as C
import           Data.Conduit.Attoparsec (sinkParser)
import qualified Data.Text		 as T
import qualified Data.Text.Encoding	 as DTE
import qualified Data.Vector		 as V
import           Network.HTTP.Conduit    (Request, RequestBody (RequestBodyLBS),
					  Response (..), applyBasicAuth, http,
					  method, parseUrl, path, requestBody,
					  withManager)

import           CreateSendAPI.V3.Session (ListRequest (..))
import           CreateSendAPI.V3.Util	  (httpGetByteString)



--
-- Data Types:
--

data WebhookEvents = WebhookEvents	{ webhookEvtSubscribe :: Bool
					, webhookEvtDeactivate :: Bool
					, webhookEvtUpdate :: Bool
					}
	deriving (Show, Eq)

data WebhookPayloadFormat = WebhookFmtJSON | WebhookFmtXML
	deriving (Show, Eq)

data WebhookDetails = WebhookDetails
		    { webhookID :: T.Text
		    , webhookEvents :: WebhookEvents
		    , webhookURL :: T.Text
		    , webhookStatus :: T.Text
		    , webhookPayloadFormat :: WebhookPayloadFormat
		    } deriving (Show, Eq)



--
-- Data Type Instances, and Utility Functions:
--

webhookEventsToStrList :: WebhookEvents -> [String]
webhookEventsToStrList (WebhookEvents {..}) = l3
  where l1 = case webhookEvtUpdate of
  		True -> ["Update"]
		_ -> []
	l2 = case webhookEvtDeactivate of
		True -> "Deactivate":l1
		_ -> l1
	l3 = case webhookEvtSubscribe of
		True -> "Subscribe":l2
		_ -> l2

webhookPayloadFormatToStr :: WebhookPayloadFormat -> String
webhookPayloadFormatToStr WebhookFmtJSON = "json"
webhookPayloadFormatToStr WebhookFmtXML = "xml"


instance FromJSON WebhookEvents where
    parseJSON j = do
	o <- parseJSON j
	res <- V.foldM procsEvtObj (WebhookEvents False False False) o
	return res
      where procsEvtObj accum o = do
      		evtName <- parseJSON o
		case evtName of
      	    	  "Subscribe"  -> return $ accum { webhookEvtSubscribe  = True }
	    	  "Deactivate" -> return $ accum { webhookEvtDeactivate = True }
		  "Update"     -> return $ accum { webhookEvtUpdate     = True }
		  _ -> fail $ "Unrecognized event name: " ++ evtName

instance FromJSON WebhookPayloadFormat where
    parseJSON j = do
    	o <- parseJSON j
	case (o :: String) of
	  "Json" -> return WebhookFmtJSON
	  "Xml" -> return WebhookFmtXML
	  _ -> fail $ "Unrecognized webhook payload format: " ++ (show o)

instance FromJSON WebhookDetails where
    parseJSON (Object v) = WebhookDetails <$>
    				v .: "WebhookID" <*>
				v .: "Events" <*>
				v .: "Url" <*>
				v .: "Status" <*>
				v .: "PayloadFormat"
    parseJSON _ = fail "Expected a JSON object when parsing WebhookDetails."



--
-- HTTP Methods:
--

getWebhooksJSON (ListRequest listReq) = withManager $ \manager -> do
        let req = listReq { path = (path listReq) `BS.append` "/webhooks.json"
		          , method = "GET"
		          }
	res <- http req manager
	responseBody res $$+- sinkParser json

getWebhooks listReq = do
	v <- getWebhooksJSON listReq
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: V.Vector WebhookDetails)

createWebhook (ListRequest req) webhookEvents webhookURL payloadFormat =
    httpGetByteString $
	req { path = (path req) `BS.append` "/webhooks.json"
	    , method = "POST"
	    , requestBody = RequestBodyLBS $ encode $ object [
		"Events" .= webhookEventsToStrList webhookEvents,
		"Url" .= webhookURL,
		"PayloadFormat" .= webhookPayloadFormatToStr payloadFormat]
	    }

activateWebhook (ListRequest req) webhookID = httpGetByteString $
	req { path = (path req) `BS.append` "/webhooks/" `BS.append`
		webhookID `BS.append` "/activate.json"
	    , method = "PUT"
	    }

deactivateWebhook (ListRequest req) webhookID = httpGetByteString $
	req { path = (path req) `BS.append` "/webhooks/" `BS.append`
		webhookID `BS.append` "/deactivate.json"
	    , method = "PUT"
	    }

deleteWebhook (ListRequest req) webhookID = httpGetByteString $
	req { path = (path req) `BS.append` "/webhooks/" `BS.append`
		webhookID `BS.append` ".json"
	    , method = "DELETE"
	    }

testWebhook (ListRequest req) webhookID = httpGetByteString $
	req { path = (path req) `BS.append` "/webhooks/" `BS.append`
		webhookID `BS.append` "/test.json"
	    , method = "GET"
	    }
