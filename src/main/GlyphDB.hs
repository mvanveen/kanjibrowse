{-# LANGUAGE OverloadedStrings #-}
module GlyphDB (loadGlyphDB) where

import Control.Monad (filterM)
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CL (filterM)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import System.Directory
import System.FilePath ((</>))

import Glyph

type GlyphDB = Map.HashMap Char [Glyph]

loadGlyphDB :: FilePath -> IO GlyphDB
loadGlyphDB path = do
    glyphs <- loadGlyphs path
    putStrLn $ "number of glyphs loaded: " ++ show (length glyphs)
    let db = Map.fromListWith (++) $ map indexer glyphs
    putStrLn $ "number of DB entries: " ++ show (Map.size db)
    return db

indexer :: Glyph -> (Char, [Glyph])
indexer glyph = (index, [glyph])
  where
    index = fromMaybe '\0' $ glyphName glyph

loadGlyphs :: FilePath -> IO [Glyph]
loadGlyphs path = do
    list <- getDirectoryContents path
    CL.sourceList list $$ pipeline $= CL.consume
  where
    pipeline = prependBase
           =$= filterFiles
           =$= readFiles
           =$= parseGlyphs
    prependBase = CL.map (path </>)
    filterFiles = CL.filterM doesFileExist
    readFiles = CL.mapM readFile
    parseGlyphs = CL.mapMaybe parseKvg
