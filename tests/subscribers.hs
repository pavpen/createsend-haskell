{-# LANGUAGE OverloadedStrings #-}


import 		 CreateSendAPI.V3
import 		 Data.Aeson.Types	(Result (..))
import qualified Data.ByteString.Char8	as BSC8
import 		 Data.Default		(def)
import qualified Data.Foldable		as F
import 		 System.IO		(hFlush, stdout)


prompt p = do
    putStr p
    hFlush stdout
    getLine

main :: IO ()
main = do
    apiKey <- prompt "Campaign Monitor API Key: "
    listId <- prompt "List ID: "
    let authReq = reqFromAPIKey (BSC8.pack apiKey)
    let listReq = listReqFromAPIKeyReq authReq (BSC8.pack listId)
    -- | Print a page of test list's active subscribers:
    page <- getActiveSubscribers listReq def
    F.mapM_ (putStrLn . show) (subscriberResults page)
    return ()
