{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid

main = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app req = do
    let path = pathInfo req

showSVG = responseBuilder status200 [ ("Content-Type", "application/xhtml+xml") ] $ mconcat $ map copyByteString
    ["yay"]

index x = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p>Hello from ", BU.fromString $ show x, "!</p>"
    , "<p><a href='/yay'>yay</a></p>\n" ]
