{-# LANGUAGE OverloadedStrings #-}
module GlyphDB (fromKvgDir) where

import Control.Monad (filterM)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import System.Directory
import System.FilePath ((</>))
import System.IO.Strict as IO

import Glyph

type GlyphDB = Map.Map String Glyph

fromKvgDir :: FilePath -> IO GlyphDB
fromKvgDir path = do
    list <- getDirectoryContents path
    let paths = map (path </>) list
    files <- filterM doesFileExist paths
    kvgs <- mapM IO.readFile files
    let glyphs = catMaybes $ map parseKvg kvgs
    print $ length glyphs
    return $ Map.empty
