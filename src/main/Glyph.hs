{-# LANGUAGE OverloadedStrings #-}
module Glyph
    ( parseKVG
    , writeKVG
    ) where

import Text.XML.Light as X
import Text.XML.Light.Input as XI

data KVG
    = Path { pathData :: String }
    | Group
        { groupName :: Maybe String
        , groupSubGlyphs :: [Glyph]
        }

parseKVG :: String -> Maybe KVG
parseKVG = undefined

writeKVG :: KVG -> String
writeKVG = undefined
