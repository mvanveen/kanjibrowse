{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , RecordWildCards
           #-}
module Glyph
    ( parseKvg
    , writeKvg
    , renderSvg
    ) where

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes)

import Text.XML.Light as X
import Text.XML.Light.Input as XI
import Text.XML.Light.Output as XO

data Glyph
    = Path { pathData :: String }
    | Group
        { groupName :: Maybe String
        , groupSubGlyphs :: [Glyph]
        }
    deriving (Show, Eq)

xmlnsAttr = uqAttr "xmlns" "http://www.w3.org/2000/svg"
xlinkAttr = X.Attr
              X.QName{ qPrefix = Just "xmlns", qName = "xlink", qURI = Nothing }
              "http://www.w3.org/1999/xlink"

hrefName = X.QName{ qPrefix = Just "xlink", qName = "href", qURI = Nothing }

parseKvg :: String -> Maybe Glyph
parseKvg kvg = do
    xml <- XI.parseXMLDoc kvg
    parseKvgXml xml

writeKvg :: Glyph -> String
writeKvg glyph = XO.showTopElement $ kvg
  where
    kvg = X.unode "svg" (attrs, [writeKvgXml glyph])
    attrs =
      [ xmlnsAttr
      , uqAttr "width"   "109"
      , uqAttr "height"  "109"
      , uqAttr "viewBox" "0 0 109 109"
      ]

parseKvgXml :: X.Element -> Maybe Glyph
parseKvgXml xml = undefined

writeKvgXml :: Glyph -> X.Element
writeKvgXml = \case
    Path{..}  -> X.unode "path" $ uqAttr "d" pathData
    Group{..} -> X.unode "g" $ (catMaybes [uqAttr "element" <$> groupName], map writeKvgXml groupSubGlyphs)

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
renderSvgXml = \case
    Path{..}  -> X.unode "path" $ uqAttr "d" pathData
    Group{..} ->
      case groupName of
        Just element -> X.unode "a" ([X.Attr hrefName element], [X.unode "g" $ map renderSvgXml groupSubGlyphs])
        Nothing      -> X.unode "g" $ map renderSvgXml groupSubGlyphs

uqAttr :: String -> String -> X.Attr
uqAttr = X.Attr . X.unqual
