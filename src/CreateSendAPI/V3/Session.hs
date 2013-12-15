{-# LANGUAGE OverloadedStrings #-}

module CreateSendAPI.V3.Session where


import qualified Data.ByteString	as BS
import qualified Data.IP		as IP
import 		 Network.HTTP.Conduit	(Request (..), applyBasicAuth,
					 parseUrl, path)



-- | A type for Campiagn Monitor API requests that contain authentication
--   headers.
newtype AuthenticatedRequest = AuthenticatedRequest Request

-- | A type for authenticated Campaign Monitor API requests that refer to a
--   specific subscriber list.
newtype ListRequest = ListRequest Request


-- | The base HTTP request pointing to Campaign Monitor's HTTPS API.
apiReqBase :: Request
apiReqBase = let (Just req') = parseUrl "https://api.createsend.com/api/v3/"
	     in req'

-- | Create an authenticated HTTP request from a Campaign Monitor API key.
reqFromAPIKey :: BS.ByteString -> AuthenticatedRequest
reqFromAPIKey key = AuthenticatedRequest $ applyBasicAuth key "" apiReqBase

-- | Create an authenticated HTTP request referring to a CampaignMonitor list.
listReqFromAPIKeyReq :: AuthenticatedRequest -> BS.ByteString -> ListRequest
listReqFromAPIKeyReq (AuthenticatedRequest apiKeyReq) listID =
    ListRequest $ apiKeyReq {
    	path = (path apiKeyReq) `BS.append` "lists/" `BS.append` listID }


-- | IP address range for Campaign Monitor webhook servers.  This range is
--   intended to be used for such purposes as filtering incoming webhook
--   requests by IP.
apiWebhookIPRange :: IP.IPRange
apiWebhookIPRange = "103.28.40.0/22"

-- | Test if an IP is in one of the Campaign Monitor API server ranges.
isInAPIIPRange ip | ip `IP.isMatchedTo` "27.126.144.0/21"	= True
		  | ip `IP.isMatchedTo` "103.28.40.0/22"	= True
		  | ip `IP.isMatchedTo` "180.189.136.0/22"	= True
		  | ip `IP.isMatchedTo` "184.106.86.128/28"	= True
		  | ip `IP.isMatchedTo` "184.106.87.160/29"	= True
		  | ip `IP.isMatchedTo` "184.106.87.168/29"	= True
		  | ip `IP.isMatchedTo` "203.55.21.0/24"	= True
		  | ip `IP.isMatchedTo` "204.75.142.0/24"	= True
		  | ip `IP.isMatchedTo` "205.166.177.0/24"	= True
		  | ip `IP.isMatchedTo` "206.72.127.0/24"	= True
		  | otherwise = False

-- | Test if an IP is in one of Campaign Monitor's e-mail server ranges.
isInSMTPdIPRange ip | ip `IP.isMatchedTo` "27.126.148.96/27"	= True
		    | ip `IP.isMatchedTo` "27.126.147.96/27"	= True
		    | ip `IP.isMatchedTo` "27.126.146.96/27"	= True
		    | ip `IP.isMatchedTo` "205.166.177.0/24"	= True
		    | ip `IP.isMatchedTo` "204.75.142.0/24"	= True
		    | ip `IP.isMatchedTo` "203.55.21.0/24"	= True
		    | ip `IP.isMatchedTo` "103.28.42.0/24"	= True
		    | otherwise = False
