{-# LANGUAGE OverloadedStrings #-}


import 		 CreateSendAPI.V3
import 		 Data.Aeson.Types	(Result (..))
import qualified Data.ByteString.Char8	as BSC8
import 		 Data.Default		(def)
import qualified Data.Foldable		as F

main :: IO ()
main = do
    putStr "Campaign Monitor API Key: "
    apiKey <- getLine
    putStr "List ID: "
    listId <- getLine
    let authReq = reqFromAPIKey (BSC8.pack apiKey)
    let listReq = listReqFromAPIKeyReq authReq (BSC8.pack listId)
    -- | Print a page of test list's active subscribers:
    page <- getActiveSubscribers listReq def
    F.mapM_ (putStrLn . show) (subscriberResults page)
    return ()
