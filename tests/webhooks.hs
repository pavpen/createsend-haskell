{-# LANGUAGE OverloadedStrings #-}


import 		 CreateSendAPI.V3
import 		 Data.Aeson.Types	(Result (..))
import qualified Data.ByteString.Char8	as BSC8
import qualified Data.Foldable		as F

main :: IO ()
main = do
    putStr "Campaign Monitor API Key: "
    apiKey <- getLine
    putStr "List ID: "
    listId <- getLine
    let authReq = reqFromAPIKey (BSC8.pack apiKey)
    let listReq = listReqFromAPIKeyReq authReq (BSC8.pack listId)
    -- | Print the test list's web hooks:
    hooks <- getWebhooks listReq
    F.mapM_ (putStrLn . show) hooks
    return ()
