{-# LANGUAGE	  DoAndIfThenElse
 		, FlexibleContexts
		, GADTs
		, OverloadedStrings
		, RecordWildCards #-}

module CreateSendAPI.V3.List
	( module CreateSendAPI.V3.List.Subscribers
	, module CreateSendAPI.V3.List.Webhooks
	, module CreateSendAPI.V3.List
	) where


import 		 Control.Applicative		((<$>), (<*>))
import 		 Control.Monad.IO.Class		(MonadIO)
import 		 Data.Aeson			(FromJSON (parseJSON),
						 Value (Object), (.:), (.=),
						 encode, fromJSON, object)
import		 Data.Aeson.Parser		(json)
import 		 Data.Aeson.Types		(Result (..))
import qualified Data.ByteString		as BS
import qualified Data.ByteString.Internal	as B
import 		 Data.Conduit			(($$+-))
import qualified Data.Conduit			as C
import 		 Data.Conduit.Attoparsec	(sinkParser)
import qualified Data.Text			as T
import qualified Data.Vector			as V
import 		 Network.HTTP.Conduit		(RequestBody (RequestBodyLBS),
						 Response (..), http, method,
						 path, requestBody,
						 withManager)

import 		 CreateSendAPI.V3.List.Subscribers
import           CreateSendAPI.V3.List.Webhooks
import 		 CreateSendAPI.V3.Session	(AuthenticatedRequest (..),
						 ListRequest (..))
import		 CreateSendAPI.V3.Util		(httpGetByteString,
						 httpGetJSON, sinkByteString)



--
-- Data Types:
--

data ListUnsubscribeSetting = ListUnsubscribeAll | ListUnsubscribeOnlyThis
	deriving (Show, Eq)

data ListDetails = ListDetails
		 { listID :: BS.ByteString
		 , listTitle :: T.Text
		 , listUnsubscribePage :: T.Text
		 , listConfirmedOptIn :: Bool
		 , listConfirmationSuccessPage :: T.Text
		 , listUnsubscribeSetting :: ListUnsubscribeSetting
		 } deriving (Show, Eq)

data ListStats = ListStats
	       { listTotalActiveSubscribers :: Integer
	       , listNewActiveSubscribersToday :: Integer
	       , listNewActiveSubscribersYesterday :: Integer
	       , listNewActiveSubscribersThisWeek :: Integer
	       , listNewActiveSubscribersThisMonth :: Integer
	       , listNewActiveSubscribersThisYear :: Integer
	       , listTotalUnsubscribers :: Integer
	       , listUnsubscribersToday :: Integer
	       , listUnsubscribersYesterday :: Integer
	       , listUnsubscribersThisWeek :: Integer
	       , listUnsubscribersThisMonth :: Integer
	       , listUnsubscribersThisYear :: Integer
	       , listTotalDeleted :: Integer
	       , listDeletedToday :: Integer
	       , listDeletedYesterday :: Integer
	       , listDeletedThisWeek :: Integer
	       , listDeletedThisMonth :: Integer
	       , listDeletedThisYear :: Integer
	       , listTotalBounces :: Integer
	       , listBouncesToday :: Integer
	       , listBouncesYesterday :: Integer
	       , listBouncesThisWeek :: Integer
	       , listBouncesThisMonth :: Integer
	       , listBouncesThisYear :: Integer
	       } deriving (Show, Eq)

data ListCustomFieldDetails = ListCustomFieldDetails
			    { fieldName :: T.Text
			    , fieldKey :: T.Text
			    , fieldDataType :: T.Text
			    , fieldOptions :: V.Vector T.Text
			    , fieldVisible :: Bool
			    } deriving (Show, Eq)

data ListSegmentDetails = ListSegmentDetails
			{ segmentListID :: BS.ByteString
			, segmentID :: BS.ByteString
			, segmentTitle :: T.Text
			} deriving (Show, Eq)



--
-- Data Type Instances, and Utility Functions:
--

unsubscribeSettingToStr :: ListUnsubscribeSetting -> String
unsubscribeSettingToStr ListUnsubscribeAll = "AllClientLists"
unsubscribeSettingToStr ListUnsubscribeOnlyThis = "OnlyThisList"

instance FromJSON ListUnsubscribeSetting where
    parseJSON j = do
    	o <- parseJSON j
	case (o :: String) of
	  "AllClientLists" -> return ListUnsubscribeAll
	  "OnlyThisList" -> return ListUnsubscribeOnlyThis
	  _ -> fail $ "Unrecognized list unsubscribe setting: " ++ (show o)


instance FromJSON ListDetails where
    parseJSON (Object v) = ListDetails <$>
				v .: "ListID" <*>
				v .: "Title" <*>
				v .: "UnsubscribePage" <*>
    				v .: "ConfirmedOptIn" <*>
				v .: "ConfirmationSuccessPage" <*>
				v .: "UnsubscribeSetting"
    parseJSON _ = fail "Can't parse a non-JSON object as a ListDetail!"

instance FromJSON ListStats where
    parseJSON (Object v) = ListStats <$>
				v .: "TotalActiveSubscribers" <*>
				v .: "NewActiveSubscribersToday" <*>
				v .: "NewActiveSubscribersYesterday" <*>
    				v .: "NewActiveSubscribersThisWeek" <*>
				v .: "NewActiveSubscribersThisMonth" <*>
				v .: "NewActiveSubscribersThisYear" <*>
				v .: "TotalUnsubscribers" <*>
				v .: "UnsubscribersToday" <*>
				v .: "UnsubscribersYesterday" <*>
				v .: "UnsubscribersThisWeek" <*>
				v .: "UnsubscribersThisMonth" <*>
				v .: "UnsubscribersThisYear" <*>
				v .: "TotalDeleted" <*>
				v .: "DeletedToday" <*>
				v .: "DeletedYesterday" <*>
				v .: "DeletedThisWeek" <*>
				v .: "DeletedThisMonth" <*>
				v .: "DeletedThisYear" <*>
				v .: "TotalBounces" <*>
				v .: "BouncesToday" <*>
				v .: "BouncesYesterday" <*>
				v .: "BouncesThisWeek" <*>
				v .: "BouncesThisMonth" <*>
				v .: "BouncesThisYear"
    parseJSON _ = fail "Can't parse a non-JSON object as ListStats!"

instance FromJSON ListCustomFieldDetails where
    parseJSON (Object v) = ListCustomFieldDetails <$>
    				v .: "FieldName" <*>
				v .: "Key" <*>
				v .: "DataType" <*>
				v .: "FieldOptions" <*>
				v .: "VisibleInPreferenceCenter"
    parseJSON _ = fail "Can't parse a non-JSON object as a ListCustomField!"

instance FromJSON ListSegmentDetails where
    parseJSON (Object v) = ListSegmentDetails <$>
    				v .: "ListID" <*>
				v .: "SegmentID" <*>
				v .: "Title"
    parseJSON _ = fail "Can't parse a non-JSON object as a ListSegmentDetails!"



--
-- HTTP Methods:
--

createList :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
	       C.MonadThrow m, r ~ C.ResourceT m)
	      => AuthenticatedRequest r -> BS.ByteString -> T.Text -> T.Text
	      -> Bool -> T.Text -> ListUnsubscribeSetting
	      -> m (Maybe B.ByteString)
createList (AuthenticatedRequest req) clientID title unsubscribePage confirmedOptIn confirmationSuccessPage unsubscribeSetting = httpGetByteString $
    req { path = (path req) `BS.append` "lists/" `BS.append` 
	clientID `BS.append` ".json"
	, method = "POST"
	, requestBody = RequestBodyLBS $ encode $ object [
	    "Title" .= title
	    , "UnsubscribePage" .= unsubscribePage
	    , "UnsubscribeSetting" .= unsubscribeSettingToStr unsubscribeSetting
	    , "ConfirmedOptIn" .= confirmedOptIn
	    , "ConfirmationSuccessPage" .= confirmationSuccessPage
	  ]
	}

getListDetailsJSON (ListRequest req) = httpGetJSON $
    req { path = (path req) `BS.append` ".json"
	, method = "GET"
	}

getListDetails req = do
	v <- getListDetailsJSON req
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: ListDetails)


getListStatsJSON (ListRequest req) = httpGetJSON $
    req { path = (path req) `BS.append` "/stats.json"
	, method = "GET"
	}

getListStats req = do
	v <- getListStatsJSON req
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: ListStats)


getListCustomFieldsJSON (ListRequest req) = httpGetJSON $
    req { path = (path req) `BS.append` "/customfields.json"
	, method = "GET"
	}

getListCustomFields req = do
	v <- getListCustomFieldsJSON req
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: V.Vector ListCustomFieldDetails)


getListSegmentDetailsJSON (ListRequest req) = httpGetJSON $
    req { path = (path req) `BS.append` "/segments.json"
	, method = "GET"
	}

getListSegmentDetails req = do
	v <- getListSegmentDetailsJSON req
	case fromJSON v of
	  Error msg -> fail msg
	  Success res -> return (res :: V.Vector ListSegmentDetails)
