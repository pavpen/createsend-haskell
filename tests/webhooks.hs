{-# LANGUAGE OverloadedStrings #-}


import 		 CreateSendAPI.V3
import 		 Data.Aeson.Types	(Result (..))
import qualified Data.ByteString.Char8	as BSC8
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
    -- | Print the test list's web hooks:
    hooks <- getWebhooks listReq
    F.mapM_ (putStrLn . show) hooks
    return ()
