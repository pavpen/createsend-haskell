{-# LANGUAGE	  DoAndIfThenElse
 		, FlexibleContexts
		, GADTs
		, OverloadedStrings
		, RecordWildCards #-}

module CreateSendAPI.V3.Subscriber where

import 		 Control.Applicative		((<$>), (<*>))
import           Data.Aeson			(FromJSON, (.:))
import qualified Data.Aeson			as JSON
import qualified Data.Text			as T
import           Data.Time.Clock		(UTCTime)
import qualified Data.Vector			as V


--
-- Data Types:
--

data State = Active | Unconfirmed | Unsubscribed | Bounced | Deleted
	deriving (Show, Eq)

data CustomFieldValue = CustomFieldValue
		      { key :: T.Text
		      , value :: JSON.Value
		      } deriving (Show, Eq)

data Details = Details
	{ emailAddress :: T.Text
	, name :: T.Text
	, date :: UTCTime
	, state :: State
	, customFields :: V.Vector CustomFieldValue
	, readsEmailWith :: T.Text
	} deriving (Show, Eq)

data OrderField = ByEmail | ByName | ByDate deriving (Show, Eq)



--
-- Data Type Instances, and Utility Functions:
--

stateToStr Active = "Active"
stateToStr Unconfirmed = "Unconfirmed"
stateToStr Unsubscribed = "Unsubscribed"
stateToStr Bounced = "Bounced"
stateToStr Deleted = "Deleted"

instance FromJSON State where
    parseJSON (JSON.String "Active") = return Active
    parseJSON (JSON.String "Unconfirmed") = return Unconfirmed
    parseJSON (JSON.String "Unsubscribed") = return Unsubscribed
    parseJSON (JSON.String "Bounced") = return Bounced
    parseJSON (JSON.String "Deleted") = return Deleted
    parseJSON v = fail $ "Unrecognized JSON value: " ++ (show v)


instance FromJSON CustomFieldValue where
    parseJSON (JSON.Object v) = CustomFieldValue <$>
    				  v .: "Key" <*>
				  v .: "Value"
    parseJSON _ = fail "Expected a JSON object when parsing a CustomFieldValue!"


instance FromJSON Details where
    parseJSON (JSON.Object v) = Details <$>
    				v .: "EmailAddress" <*>
				v .: "Name" <*>
				v .: "Date" <*>
				v .: "State" <*>
				v .: "CustomFields" <*>
				v .: "ReadsEmailWith"


orderFieldToStr ByEmail = "email"
orderFieldToStr ByName = "name"
orderFieldToStr ByDate = "date"

instance FromJSON OrderField where
    parseJSON (JSON.String "email") = return ByEmail
    parseJSON (JSON.String "name") = return ByName
    parseJSON (JSON.String "date") = return ByDate
    parseJSON v = fail $ "Unrecognized JSON value when parsing an OrderField: " ++ (show v)
