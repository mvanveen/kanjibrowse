{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , RecordWildCards
           #-}
module Glyph
    ( Glyph(..)
    , parseKvg
    , writeKvg
    , renderSvgXml
    , renderXhtml
    , glyphName
    ) where

import Control.Applicative ((<$>))
import Control.DeepSeq (force, NFData(..))
import Data.Generics.Aliases (orElse)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)

import Text.XML.Light as X
import Text.XML.Light.Input as X
import Text.XML.Light.Output as X
import Text.XML.Light.Proc as X

data Glyph
    = Path { pathData :: String }
    | Group
        { groupName :: Maybe String
        , groupSubGlyphs :: [Glyph]
        }
    deriving (Show, Eq)

instance NFData Glyph where
  rnf = \case
    Path{..}  -> rnf pathData
    Group{..} -> rnf groupName `seq` rnf groupSubGlyphs

glyphName :: Glyph -> Maybe Char
glyphName = \case
  Group{..} -> head <$> groupName
  Path{..}  -> Nothing

xmlnsAttr = uqAttr "xmlns" "http://www.w3.org/2000/svg"
xlinkAttr = X.Attr (xmlnsName "xlink") "http://www.w3.org/1999/xlink"

xmlnsName = prefixName "xmlns"
xlinkName = prefixName "xlink"
kvgName = prefixName "kvg"

prefixName :: String -> String -> X.QName
prefixName prefix name = X.QName{ qPrefix = Just prefix, qName = name, qURI = Nothing }

parseKvg :: String -> Maybe Glyph
parseKvg kvg = do
    xml <- X.parseXMLDoc kvg
    parseKvgXml xml

writeKvg :: Glyph -> String
writeKvg glyph = X.showTopElement $ kvg
  where
    kvg = X.unode "svg" (attrs, [writeKvgXml glyph])
    attrs =
      [ xmlnsAttr
      , uqAttr "width"  $ show size
      , uqAttr "height" $ show size
      , uqAttr "viewBox" "0 0 109 109"
      ]
    size = 109

parseKvgXml :: X.Element -> Maybe Glyph
parseKvgXml xml = do
    svg <- filterName (("svg" ==) . X.qName) xml
    strokePaths <- X.filterElement isStrokePathGroup svg
    parseKvgGlyph $ head $ X.elChildren $ strokePaths

filterName :: (X.QName -> Bool) -> X.Element -> Maybe X.Element
filterName pred elem = if pred $ X.elName elem then Just elem else Nothing

isStrokePathGroup :: X.Element -> Bool
isStrokePathGroup elem = X.qName (X.elName elem) == "g" && idHasPrefix "kvg:StrokePaths" elem

idHasPrefix :: String -> X.Element -> Bool
idHasPrefix prefix elem =
    case X.findAttr (X.unqual "id") elem of
        Just id -> isPrefixOf prefix id
        Nothing -> False

parseKvgGlyph :: X.Element -> Maybe Glyph
parseKvgGlyph elem = parseKvgPath elem `orElse` parseKvgGroup elem

parseKvgPath :: X.Element -> Maybe Glyph
parseKvgPath elem = do
    path <- filterName (("path" ==) . X.qName) elem
    d <- X.findAttr (X.unqual "d") path
    return $ Path{ pathData = d }

parseKvgGroup :: X.Element -> Maybe Glyph
parseKvgGroup elem = do
    g <- filterName (("g" ==) . X.qName) elem
    let name = X.findAttr (kvgName "element") g
        subGlyphs = catMaybes $ map parseKvgGlyph $ X.elChildren g
    return $ Group{ groupName = name, groupSubGlyphs = subGlyphs }

writeKvgXml :: Glyph -> X.Element
writeKvgXml = \case
    Path{..}  -> X.unode "path" $ uqAttr "d" pathData
    Group{..} -> X.unode "g" $ (catMaybes [uqAttr "element" <$> groupName], map writeKvgXml groupSubGlyphs)

renderSvgXml :: Glyph -> X.Element
renderSvgXml glyph = X.unode "svg" (attrs, [style, g])
  where
    g = X.unode "g" (uqAttr "class" "top", renderSvgXml' glyph)
    style = X.unode "style" (aStyle ++ pathStyle)
    aStyle = "a:hover{stroke:red;stroke-width:4;}" :: String
    pathStyle = "g.top{fill:none;stroke:black;stroke-width:3;stroke-linecap:round;stroke-linejoin:round;}"
    attrs =
      [ xmlnsAttr
      , xlinkAttr
      , uqAttr "width"  $ show size
      , uqAttr "height" $ show size
      , uqAttr "viewBox" "0 0 109 109"
      ]
    size = 0.3 * 109

renderSvgXml' :: Glyph -> X.Element
renderSvgXml' = \case
    Path{..}  -> X.unode "path" $ uqAttr "d" pathData
    Group{..} -> X.unode "g" $ map renderSvgXml' groupSubGlyphs

renderXhtml :: Glyph -> X.Element
renderXhtml = \case
  glyph@Group{..} -> X.unode "span" $ [renderSvgXml glyph, X.unode "ol" $ map (X.unode "li" . renderXhtml) groupSubGlyphs]
  glyph@Path{..}  -> X.unode "span" $ renderSvgXml glyph

uqAttr :: String -> String -> X.Attr
uqAttr = X.Attr . X.unqual
