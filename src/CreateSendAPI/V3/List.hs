{-# LANGUAGE	  DoAndIfThenElse
 		, FlexibleContexts
		, GADTs
		, OverloadedStrings
		, RecordWildCards #-}

module CreateSendAPI.V3.List
	( module CreateSendAPI.V3.List.Webhooks
	, module CreateSendAPI.V3.List
	) where


import 		 Control.Monad.IO.Class		(MonadIO)
import 		 Data.Aeson			((.=), encode, object)
import qualified Data.ByteString		as BS
import qualified Data.ByteString.Internal	as B
import 		 Data.Conduit			(($$+-))
import qualified Data.Conduit			as C
import qualified Data.Text			as T
import 		 Network.HTTP.Conduit		(RequestBody (RequestBodyLBS),
						 Response (..), http, method,
						 path, requestBody,
						 withManager)

import           CreateSendAPI.V3.List.Webhooks
import 		 CreateSendAPI.V3.Session	(AuthenticatedRequest (..))
import		 CreateSendAPI.V3.Util		(sinkByteString)


data ListUnsubscribeSetting = ListUnsubscribeAll | ListUnsubscribeOnlyThis
	deriving (Show, Eq)


unsubscribeSettingToStr :: ListUnsubscribeSetting -> String
unsubscribeSettingToStr ListUnsubscribeAll = "AllClientLists"
unsubscribeSettingToStr ListUnsubscribeOnlyThis = "OnlyThisList"


createList :: (MonadIO m, C.MonadBaseControl IO m, C.MonadUnsafeIO m,
	       C.MonadThrow m, r ~ C.ResourceT m)
	      => AuthenticatedRequest r -> BS.ByteString -> T.Text -> T.Text
	      -> Bool -> T.Text -> ListUnsubscribeSetting
	      -> m (Maybe B.ByteString)
createList (AuthenticatedRequest req) clientID title unsubscribePage confirmedOptIn confirmationSuccessPage unsubscribeSetting =
    withManager $ \manager -> do
    	let req' = req {
		     path = (path req) `BS.append` "lists/" `BS.append` 
		     	clientID `BS.append` ".json"
		     , method = "POST"
		     , requestBody = RequestBodyLBS $ encode $ object [
		     	  "Title" .= title
			, "UnsubscribePage" .= unsubscribePage
			, "UnsubscribeSetting" .=
				unsubscribeSettingToStr unsubscribeSetting
			, "ConfirmedOptIn" .= confirmedOptIn
			, "ConfirmationSuccessPage" .= confirmationSuccessPage
		     	]
		     }
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)
