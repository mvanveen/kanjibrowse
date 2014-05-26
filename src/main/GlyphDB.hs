{-# LANGUAGE OverloadedStrings
           , LambdaCase
           , RecordWildCards
           #-}
module GlyphDB (loadGlyphDB, (Map.!)) where

import Control.Monad (filterM)
import Control.Exception (evaluate)
import Control.DeepSeq (force, NFData(..))
import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Combinators as CL (filterM)
import qualified Data.HashMap.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Traversable as T
import System.Directory
import System.FilePath ((</>))
import System.IO.Strict as IO

import Glyph

type GlyphDB = Map.HashMap Char [Glyph]

loadGlyphDB :: FilePath -> IO GlyphDB
loadGlyphDB path = do
    glyphs <- loadGlyphs path
    let db = Map.fromListWith (++) $ map indexer glyphs
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
    readFiles = CL.mapM IO.readFile -- IO.readFile is strict; file handles get recycled ASAP
    parseGlyphs = CL.mapMaybeM $ T.mapM (evaluate . force) . parseKvg
