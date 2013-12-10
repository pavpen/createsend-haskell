{-# LANGUAGE OverloadedStrings #-}

module CreateSendAPI.V3.Webhooks where


import 		 Control.Applicative	((<$>), (<*>))
import qualified CreateSendAPI.V3.Subscriber	as S
import 		 Data.Aeson		(FromJSON (parseJSON),
					 Value (Object), (.:), (.=))
import qualified Data.Aeson.Types	as AT
import qualified Data.ByteString	as BS
import qualified Data.Text		as T
import 		 Data.Time.Clock	(UTCTime)
import 		 Data.Vector		(Vector)


--
-- Data Types:
--
data EventBatch = EventBatch
		{ events :: Vector EventParams
		, listID :: BS.ByteString
		}


data EventParams =
	  SubscribeParams  { subscribeCustomFields :: Vector S.CustomFieldValue
			   , subscribeDate :: UTCTime
			   , subscribeEmail :: T.Text
			   , subscribeName :: T.Text
			   , subscribeIP :: T.Text
			   }
	| UpdateParams     { updateCustomFields :: Vector S.CustomFieldValue
			   , updateDate :: UTCTime
			   , updateOldEmail :: T.Text
			   , updateEmail :: T.Text
			   , updateName :: T.Text
			   , updateState :: T.Text
			   }
	| DeactivateParams { deactivateCustomFields :: Vector S.CustomFieldValue
			   , deactivateDate :: UTCTime
			   , deactivateEmail :: T.Text
			   , deactivateName :: T.Text
			   , deactivateState :: T.Text
			   }



-- 
-- Instance implementations:
--
instance FromJSON EventBatch where
    parseJSON (Object v) = EventBatch <$>
    				v .: "Events" <*>
				v .: "ListID"

instance FromJSON EventParams where
    parseJSON (Object v) = do
    	evtType <- v .: "Type"
    	parseType evtType v
      where parseType :: T.Text -> AT.Object -> AT.Parser EventParams
            praseType "Subscribe" v = SubscribeParams <$>
      					v .: "CustomFields" <*>
					v .: "Date" <*>
					v .: "EmailAddress" <*>
					v .: "Name" <*>
					v .: "SignupIPAddress"
	    parseType "Update" v = UpdateParams <$>
	    				v .: "CustomFields" <*>
					v .: "Date" <*>
					v .: "OldEmailAddress" <*>
					v .: "EmailAddress" <*>
					v .: "Name" <*>
					v .: "State"
	    parseType "Deactivate" v = DeactivateParams <$>
	    				v .: "CustomFields" <*>
					v .: "Date" <*>
					v .: "EmailAddress" <*>
					v .: "Name" <*>
					v .: "State"
