{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , RecordWildCards
           #-}
module Glyph
    ( parseKvg
    , writeKvg
    , renderSvg
    ) where

import Text.XML.Light as X
import Text.XML.Light.Input as XI
import Text.XML.Light.Output as XO

data Glyph
    = Path { pathData :: String }
    | Group
        { groupName :: Maybe String
        , groupSubGlyphs :: [Glyph]
        }

xmlnsAttr = uqAttr "xmlns" "http://www.w3.org/2000/svg"
xlinkAttr = X.Attr
              X.QName{ qName = "xlink", qURI = Nothing, qPrefix = "xmlns" }
              "http://www.w3.org/1999/xlink"

parseKvg :: String -> Maybe Glyph
parseKvg kvg = do
    xml <- XI.parseXMLDoc kvg
    return $  xml

writeKvg :: Glyph -> String
writeKvg glyph = XO.showTopElement $ kvg
  where
    kvg = X.unode "svg" (attrs, writeKvgXml glyph)
    attrs =
      [ xmlnsAttr
      , uqAttr "width"   "109"
      , uqAttr "height"  "109"
      , uqAttr "viewBox" "0 0 109 109"
      ]

parseKvgXml :: X.Element -> Maybe Glyph
parseKvgXml xml

writeKvgXml :: Glyph -> X.Element
writeKvgXml = \case
    Path{..}  -> X.unode "path" $ mkAttr "d" pathData
    Group{..} -> X.unode "g" $ ([mkAttr "element" groupName], map writeXML groupSubGlyphs)

renderSvg :: Glyph -> String
renderSvg glyph = XO.showTopElement $ svg
  where
    svg = X.unode "svg" (attrs, renderSvgXml glyph)
    attrs =
      [ xmlnsAttr
      , xlinkAttr
      , uqAttr "width"   "109"
      , uqAttr "width"   "109"
      , uqAttr "viewBox" "0 0 109 109"
      ]

renderSvgXml :: Glyph -> X.Element
renderSvgXml = undefined

uqAttr :: String -> String -> X.Attr
uqAttr = X.Attr . X.unqual
