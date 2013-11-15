{-# LANGUAGE	  DoAndIfThenElse
 		, FlexibleContexts
		, OverloadedStrings
		, RecordWildCards #-}

module CreateSendAPI.V3.List.Subscribers where

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

import 		 CreateSendAPI.V3.PagedResult
					  (SubscriberResultsPage,
					   subscriberQueryParamsToStr)
import           CreateSendAPI.V3.Session (ListRequest (..))
import           CreateSendAPI.V3.Util	  (httpGetByteString, httpGetJSON,
					   sinkByteString)


--
-- HTTP Methods:
--

getActiveSubscribersJSON (ListRequest req) qParams  = httpGetJSON $
    req { path = (path req) `BS.append` "/active.json?" `BS.append`
    		(B.pack $ subscriberQueryParamsToStr qParams)
	, method = "GET"
	}

getActiveSubscribers req qParams = do
	v <- getActiveSubscribersJSON req qParams
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: SubscriberResultsPage)
