{-# LANGUAGE   DoAndIfThenElse
 	     , OverloadedStrings
   	     , RecordWildCards #-}

module CreateSendAPI.V3.PagedResult where

import 		 Control.Applicative		((<$>), (<*>))
import           Data.Aeson			(FromJSON, (.:))
import qualified Data.Aeson			as JSON
import 		 Data.Default			(Default (..))
import 		 Data.Time.Calendar		(Day, showGregorian)
import qualified Data.Vector			as V

import qualified CreateSendAPI.V3.Subscriber	as Subscriber



--
-- Data Types:
--

data OrderDirection = Asc | Desc deriving (Show, Eq)

data SubscriberResultsPage = SubscriberResultsPage
	{ subscriberResults :: V.Vector Subscriber.Details
	, subscriberResultsOrderedBy :: Subscriber.OrderField
	, subscriberOrderDirection :: OrderDirection
	, subscriberPageNumber :: Integer
	, subscriberPageSize :: Integer
	, subscriberRecordsOnThisPage :: Integer
	, subscriberTotalNumberOfRecords :: Integer
	, subscriberNumberOfPages :: Integer
	} deriving (Show, Eq)

data SubscriberQueryParams = SubscriberQueryParams
	{ subscriberGetFromDate :: Maybe Day
	, subscriberGetPageNumber :: Integer
	, subscriberGetPageSize :: Integer
	, subscriberGetOrderField :: Subscriber.OrderField
	, subscriberGetOrderDirection :: OrderDirection
	} deriving (Show, Eq)



--
-- Data Type Instances, and Utility Functions:
--

orderDirectionToStr Asc = "asc"
orderDirectionToStr Desc = "desc"

instance FromJSON OrderDirection where
    parseJSON (JSON.String "asc") = return Asc
    parseJSON (JSON.String "desc") = return Desc
    parseJSON v = fail $ "Unrecognized OrderDirection JSON value: " ++ (show v)


instance Default SubscriberQueryParams where
    def = SubscriberQueryParams Nothing 1 1000 Subscriber.ByDate Asc

subscriberQueryParamsToStr (SubscriberQueryParams {..}) =
    date ++ "&page=" ++ (show subscriberGetPageNumber) ++
    	"&pagesize=" ++ (show subscriberGetPageSize) ++
	"&orderfield=" ++ (Subscriber.orderFieldToStr subscriberGetOrderField)++
	"&orderdirection=" ++ (orderDirectionToStr subscriberGetOrderDirection)
  where date = case subscriberGetFromDate of
  		 Nothing -> ""
		 Just day -> "date=" ++ (showGregorian day)


instance FromJSON SubscriberResultsPage where
    parseJSON (JSON.Object v) =
    	SubscriberResultsPage <$>
    	  v .: "Results" <*>
	  v .: "ResultsOrderedBy" <*>
	  v .: "OrderDirection" <*>
	  v .: "PageNumber" <*>
	  v .: "PageSize" <*>
	  v .: "RecordsOnThisPage" <*>
	  v .: "TotalNumberOfRecords" <*>
	  v .: "NumberOfPages"
    parseJSON _ = fail "Expected a JSON object when parsing a SubscriberResultsPage"
