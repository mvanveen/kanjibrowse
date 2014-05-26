{-# LANGUAGE OverloadedStrings #-}

import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import qualified Data.ByteString.UTF8 as BU
import Data.Monoid
import qualified Data.Text as T

import Glyph
import GlyphDB

main = do
    db <- loadGlyphDB "db/kanji"
    server db

server db = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port $ app db

app db req = do
    let path = pathInfo req
        root = T.head $ head $ path
        glyph = head $ db ! root
    putStrLn $ "path: " ++ show path
    putStrLn $ "lookup: " ++ [root]
    return $ svg glyph

svg glyph = responseBuilder status200 [("Content-Type", "image/svg+xml")] $ mconcat $ map copyByteString $ [BU.fromString $ renderSvg glyph]
