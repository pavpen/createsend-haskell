{-# LANGUAGE	  DoAndIfThenElse
 		, FlexibleContexts
		, OverloadedStrings
		, RecordWildCards #-}

module CreateSendAPI.V3.Lists where

import           Control.Applicative	 ((<$>), (<*>))
import           Control.Monad.IO.Class	 (MonadIO)
import           Data.Aeson		 (FromJSON (parseJSON), Value (Object),
					  (.:), (.=), encode, fromJSON, object)
import           Data.Aeson.Parser       (json)
import           Data.Aeson.Types	 (Result (..))
import qualified Data.ByteString	 as BS
import qualified Data.ByteString.Char8	 as B
import qualified Data.ByteString.Internal as B
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
import           CreateSendAPI.V3.Util	  (sinkByteString)



data WebhookEvents = WebhookEvents	{ webhookEvtSubscribe :: Bool
					, webhookEvtDeactivate :: Bool
					, webhookEvtUpdate :: Bool
					}
	deriving (Show, Eq)

data WebhookPayloadFormat = WebhookFmtJSON | WebhookFmtXML
	deriving (Show, Eq)

data Webhook = Webhook	{ webhookID :: T.Text
			, webhookEvents :: WebhookEvents
			, webhookURL :: T.Text
			, webhookStatus :: T.Text
			, webhookPayloadFormat :: WebhookPayloadFormat
			}
	deriving (Show, Eq)

data CreateWebhookParams = CreateWebhookParams
	{ createWebhookEvents :: WebhookEvents
	, createWebhookURL :: T.Text
	, createWebhookPayloadFormat :: WebhookPayloadFormat
	}

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

instance FromJSON Webhook where
    parseJSON (Object v) =	Webhook <$>
    				v .: "WebhookID" <*>
				v .: "Events" <*>
				v .: "Url" <*>
				v .: "Status" <*>
				v .: "PayloadFormat"
    parseJSON _ = fail "Expected a JSON object when parsing a Webhook."



--getWebhooksJSON :: (MonadIO m, MonadBaseControl IO m, C.MonadUnsafeIO m,
--                    C.MonadThrow m) =>
--                   ListRequest -> m Value
getWebhooksJSON (ListRequest listReq) = withManager $ \manager -> do
        let req = listReq { path = (path listReq) `BS.append` "/webhooks.json"
		          , method = "GET"
		          }
	res <- http req manager
	responseBody res $$+- sinkParser json

--getWebhooks :: (MonadIO m, MonadBaseControl IO m, C.MonadUnsafeIO m,
--                C.MonadThrow m) =>
--               ListRequest -> m (V.Vector Webhook)
getWebhooks listReq = do
	v <- getWebhooksJSON listReq
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: V.Vector Webhook)

createWebhook :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
                  C.MonadThrow m) =>
                 ListRequest r -> CreateWebhookParams -> m (Maybe B.ByteString)
createWebhook (ListRequest listReq) (CreateWebhookParams {..}) =
    withManager $ \manager -> do
	let req = listReq {
		    path = (path listReq) `BS.append` "/webhooks.json"
		    , method = "POST"
		    , requestBody = RequestBodyLBS $ encode $ object [
			"Events" .= webhookEventsToStrList createWebhookEvents,
			"Url" .= createWebhookURL,
			"PayloadFormat" .= webhookPayloadFormatToStr
				createWebhookPayloadFormat]
		    }
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)

--activateWebhook :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
--                    C.MonadThrow m, r ~ C.ResourceT m) =>
--                   ListRequest r -> B.ByteString -> m (Maybe B.ByteString)
activateWebhook (ListRequest listReq) webhookID = withManager $ \manager -> do
	let req = listReq {
		    path = (path listReq) `BS.append` "/webhooks/" `BS.append`
			webhookID `BS.append` "/activate.json"
		    , method = "PUT"
		    }
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)

--deactivateWebhook :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
--                      C.MonadThrow m) =>
--                     ListRequest r -> B.ByteString -> m (Maybe B.ByteString)
deactivateWebhook (ListRequest listReq) webhookID = withManager $ \manager -> do
	let req = listReq {
		    path = (path listReq) `BS.append` "/webhooks/" `BS.append`
			webhookID `BS.append` "/deactivate.json"
		    , method = "PUT"
		    }
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)

--deleteWebhook :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
--                  C.MonadThrow m) =>
--                 ListRequest r -> B.ByteString -> m (Maybe B.ByteString)
deleteWebhook (ListRequest listReq) webhookID = withManager $ \manager -> do
	let req = listReq {
		    path = (path listReq) `BS.append` "/webhooks/" `BS.append`
			webhookID `BS.append` ".json"
		    , method = "DELETE"
		    }
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)

--testWebhook :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
--                C.MonadThrow m) =>
--               ListRequest r -> B.ByteString -> m (Maybe B.ByteString)
testWebhook (ListRequest listReq) webhookID = withManager $ \manager -> do
	let req = listReq {
		    path = (path listReq) `BS.append` "/webhooks/" `BS.append`
			webhookID `BS.append` "/test.json"
		    , method = "GET"
		    }
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)
