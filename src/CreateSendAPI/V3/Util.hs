{-# LANGUAGE DoAndIfThenElse #-}

module CreateSendAPI.V3.Util where

import 		 CorePrelude			(liftIO)
import		 Data.Aeson.Parser		(json)
import qualified Data.ByteString.Char8		as B
import qualified Data.ByteString.Internal	as B
import 		 Data.Conduit			(($$+-))
import qualified Data.Conduit			as C
import 		 Data.Conduit.Attoparsec	(sinkParser)
import 		 Data.Time			(UTCTime, readTime)
import qualified Foreign as F
import 		 Network.HTTP.Conduit		(Response (..), http, httpLbs,
						 withManager)
import 		 System.Locale			(defaultTimeLocale)



httpGetJSON req = withManager $ \manager -> do
	res <- http req manager
	responseBody res $$+- sinkParser json

httpGetByteString req = withManager $ \manager -> do
	res <- httpLbs req manager
	let bs = responseBody res
	return bs

cmReadDate :: String -> UTCTime
cmReadDate str =
    case reads str of
	((res, _):_) -> res
	_ -> readTime defaultTimeLocale "%F" str
