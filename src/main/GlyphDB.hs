{-# LANGUAGE OverloadedStrings #-}
module GlyphDB (fromKvgDir) where

import qualified Data.Map as Map
import System.Directory
import System.IO

import Glyph

type GlyphDB = Map.Map String Glyph

fromKvgDir :: FilePath -> IO GlyphDB
fromKvgDir path = do
    files <- getDirectoryContents path
    kvgs <- mapM readFile files
