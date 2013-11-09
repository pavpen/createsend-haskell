{-# LANGUAGE OverloadedStrings #-}

module CreateSendAPI.V3.Session where


import qualified Data.ByteString as BS
import Network.HTTP.Conduit	(Request (..), applyBasicAuth, parseUrl, path)



-- | A type for Campiagn Monitor API requests that contain authentication
--   headers.
newtype AuthenticatedRequest m = AuthenticatedRequest (Request m)

-- | A type for authenticated Campaign Monitor API requests that refer to a
--   specific subscriber list.
newtype ListRequest m = ListRequest (Request m)


-- | The base HTTP request pointing to Campaign Monitor's HTTPS API.
apiReqBase :: Request m
apiReqBase = let (Just req') = parseUrl "https://api.createsend.com/api/v3/"
	     in req'

-- | Create an authenticated HTTP request from a Campaign Monitor API key.
reqFromAPIKey :: BS.ByteString -> AuthenticatedRequest m
reqFromAPIKey key = AuthenticatedRequest $ applyBasicAuth key "" apiReqBase

-- | Create an authenticated HTTP request referring to a CampaignMonitor list.
listReqFromAPIKeyReq :: AuthenticatedRequest m -> BS.ByteString -> ListRequest m
listReqFromAPIKeyReq (AuthenticatedRequest apiKeyReq) listID =
    ListRequest $ apiKeyReq {
    	path = (path apiKeyReq) `BS.append` "lists/" `BS.append` listID }
