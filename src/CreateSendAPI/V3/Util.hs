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
import 		 Network.HTTP.Conduit		(Response (..), http,
						 withManager)
import 		 System.Locale			(defaultTimeLocale)






httpGetJSON req = withManager $ \manager -> do
	res <- http req manager
	responseBody res $$+- sinkParser json

httpGetByteString req = withManager $ \manager -> do
	res <- http req manager
	responseBody res $$+- (sinkByteString 1000000)

cmReadDate :: String -> UTCTime
cmReadDate str =
    case reads str of
	((res, _):_) -> res
	_ -> readTime defaultTimeLocale "%F" str


-- sinkByteString from <http://hackage.haskell.org/package/http-conduit-downloader-1.0.3/docs/src/Network-HTTP-Conduit-Downloader.html>:
bufSize :: Int
bufSize = 32 * 1024 - overhead -- Copied from Data.ByteString.Lazy.
    where overhead = 2 * F.sizeOf (undefined :: Int)

newBuf :: IO B.ByteString
newBuf = do
    fp <- B.mallocByteString bufSize
    return $ B.PS fp 0 0

addBs :: [B.ByteString] -> B.ByteString -> B.ByteString
      -> IO ([B.ByteString], B.ByteString)
addBs acc (B.PS bfp _ bl) (B.PS sfp offs sl) = do
    let cpSize = min (bufSize - bl) sl
        bl' = bl + cpSize
    F.withForeignPtr bfp $ \ dst -> F.withForeignPtr sfp $ \ src ->
        B.memcpy (dst `F.plusPtr` bl) (src `F.plusPtr` offs) (toEnum cpSize)
    if bl' == bufSize then do
        buf' <- newBuf
--        print ("filled", cpSize)
        addBs (B.PS bfp 0 bufSize : acc) buf'
              (B.PS sfp (offs + cpSize) (sl - cpSize))
    else do
--        print ("ok", cpSize, bl')
        return (acc, B.PS bfp 0 bl')

-- | Sink data using 32k buffers to reduce memory fragmentation.
--   Returns 'Nothing' if downloaded too much data.
--sinkByteString :: MonadIO m => Int -> C.Sink B.ByteString m (Maybe B.ByteString)
sinkByteString limit = do
    buf <- liftIO $ newBuf
    go 0 [] buf
    where go len acc buf = do
              mbinp <- C.await
              case mbinp of
                  Just inp -> do
                      (acc', buf') <- liftIO $ addBs acc buf inp
--                      liftIO $ print (B.length inp)
                      let len' = len + B.length inp
                      if len' > limit then
                          return Nothing
                      else
                          go len' acc' buf'
                  Nothing -> do
--                      liftIO $ print ("len", length (buf:acc))
                      let d = B.concat $ reverse (buf:acc)
                      B.length d `seq` return $ Just d
