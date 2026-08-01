{- | A Bravura font handle with a glyph rasterization cache.

Wraps the raw "Termtab.Graphics.FreeType" FFI: loads the face once and memoizes
rendered glyphs by @(pixel-size, char)@ so the notation renderer never
re-rasterizes the same SMuFL glyph across frames. Failures (glyph not found)
are cached too, so a bad code point is only attempted once.
-}
module Termtab.Graphics.Font (
    GlyphFont,
    bravuraFontPath,
    openGlyphFont,
    withGlyphFont,
    closeGlyphFont,
    cachedGlyph,
) where

import Control.Exception (bracket)
import Data.IORef
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import System.Environment (lookupEnv)

import Termtab.Graphics.FreeType

data GlyphFont = GlyphFont
    { gfFace :: FontFace
    , gfCache :: IORef (Map (Int, Char) (Maybe GlyphBitmap))
    }

-- | The configured Bravura font path (@TERMTAB_BRAVURA_FONT@), if any.
bravuraFontPath :: IO (Maybe FilePath)
bravuraFontPath = lookupEnv "TERMTAB_BRAVURA_FONT"

-- | Load a font face and start an empty glyph cache.
openGlyphFont :: FilePath -> IO GlyphFont
openGlyphFont path = do
    face <- newFontFace path
    ref <- newIORef Map.empty
    pure (GlyphFont face ref)

-- | Release the underlying face.
closeGlyphFont :: GlyphFont -> IO ()
closeGlyphFont = freeFontFace . gfFace

-- | Bracketed 'openGlyphFont' / 'closeGlyphFont'.
withGlyphFont :: FilePath -> (GlyphFont -> IO a) -> IO a
withGlyphFont path = bracket (openGlyphFont path) closeGlyphFont

{- | Rasterize a glyph at the given pixel size, returning a cached result on
repeat calls. 'Nothing' means FreeType could not render the code point.
-}
cachedGlyph :: GlyphFont -> Int -> Char -> IO (Maybe GlyphBitmap)
cachedGlyph gf size ch = do
    cache <- readIORef (gfCache gf)
    case Map.lookup (size, ch) cache of
        Just cached -> pure cached
        Nothing -> do
            rendered <- renderGlyph (gfFace gf) size ch
            modifyIORef' (gfCache gf) (Map.insert (size, ch) rendered)
            pure rendered
