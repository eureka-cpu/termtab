{-# LANGUAGE ForeignFunctionInterface #-}

{- | Minimal FFI binding to the system FreeType library.

We deliberately bind the handful of FreeType entry points we need rather than
depend on the @freetype2@ Hackage package: that package vendors and compiles
FreeType's C source (including a bundled zlib) which does not build on the
hermetic Apple-SDK toolchain. Linking the nixpkgs system @freetype@ via
@pkgconfig-depends: freetype2@ is robust across clang versions.

This is the raw rasterization layer: it turns a glyph (a Unicode code point,
e.g. a SMuFL notehead in Bravura) into an 8-bit grayscale coverage bitmap plus
placement metrics. Glyph caching lives one layer up in "Termtab.Graphics.Font".
-}
module Termtab.Graphics.FreeType (
    FontFace,
    newFontFace,
    freeFontFace,
    withFontFace,
    GlyphBitmap (..),
    renderGlyph,
) where

import Codec.Picture.Types (Image (..), Pixel8)
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Char (ord)
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Data.Word (Word8)
import Foreign
import Foreign.C.String (CString, withCString)
import Foreign.C.Types

#include <ft2build.h>
#include FT_FREETYPE_H

type FT_Error = CInt
type FT_Library = Ptr ()
type FT_Face = Ptr ()
type FT_GlyphSlot = Ptr ()

foreign import ccall unsafe "FT_Init_FreeType"
    c_FT_Init_FreeType :: Ptr FT_Library -> IO FT_Error
foreign import ccall unsafe "FT_Done_FreeType"
    c_FT_Done_FreeType :: FT_Library -> IO FT_Error
foreign import ccall unsafe "FT_New_Face"
    c_FT_New_Face :: FT_Library -> CString -> CLong -> Ptr FT_Face -> IO FT_Error
foreign import ccall unsafe "FT_Done_Face"
    c_FT_Done_Face :: FT_Face -> IO FT_Error
foreign import ccall unsafe "FT_Set_Pixel_Sizes"
    c_FT_Set_Pixel_Sizes :: FT_Face -> CUInt -> CUInt -> IO FT_Error
foreign import ccall unsafe "FT_Load_Char"
    c_FT_Load_Char :: FT_Face -> CULong -> CInt -> IO FT_Error

-- | A loaded font face together with the library that owns it.
data FontFace = FontFace FT_Library FT_Face

-- | An 8-bit grayscale coverage bitmap plus FreeType placement metrics.
data GlyphBitmap = GlyphBitmap
    { gbImage :: Image Pixel8
    -- ^ Coverage: 0 = transparent, 255 = fully inked.
    , gbLeft :: Int
    -- ^ Horizontal bearing: pixels from the pen x to the bitmap's left edge.
    , gbTop :: Int
    -- ^ Vertical bearing: pixels from the baseline up to the bitmap's top edge.
    , gbAdvance :: Int
    -- ^ Horizontal advance in whole pixels.
    }

-- | Load a font face from a file path. Throws via 'error' on failure.
newFontFace :: FilePath -> IO FontFace
newFontFace path =
    alloca $ \libPtr -> do
        checkFT "FT_Init_FreeType" =<< c_FT_Init_FreeType libPtr
        lib <- peek libPtr
        alloca $ \facePtr -> do
            err <- withCString path $ \cpath ->
                c_FT_New_Face lib cpath 0 facePtr
            when (err /= 0) $ do
                _ <- c_FT_Done_FreeType lib
                checkFT "FT_New_Face" err
            face <- peek facePtr
            pure (FontFace lib face)

-- | Release a font face and its owning library.
freeFontFace :: FontFace -> IO ()
freeFontFace (FontFace lib face) = do
    _ <- c_FT_Done_Face face
    _ <- c_FT_Done_FreeType lib
    pure ()

-- | Bracketed 'newFontFace' / 'freeFontFace'.
withFontFace :: FilePath -> (FontFace -> IO a) -> IO a
withFontFace path = bracket (newFontFace path) freeFontFace

{- | Render a single character at the given pixel height. Returns 'Nothing'
only if FreeType could not load/render the glyph. A whitespace glyph with no
ink renders as a 0x0 image (its 'gbAdvance' is still meaningful).
-}
renderGlyph :: FontFace -> Int -> Char -> IO (Maybe GlyphBitmap)
renderGlyph (FontFace _ face) pixelHeight ch = do
    e1 <- c_FT_Set_Pixel_Sizes face 0 (fromIntegral pixelHeight)
    if e1 /= 0
        then pure Nothing
        else do
            e2 <- c_FT_Load_Char face (fromIntegral (ord ch)) (#const FT_LOAD_RENDER)
            if e2 /= 0
                then pure Nothing
                else do
                    slot <- (#peek FT_FaceRec, glyph) face :: IO FT_GlyphSlot
                    Just <$> readGlyphBitmap slot

-- | Copy the rendered bitmap out of the glyph slot into a JuicyPixels image.
readGlyphBitmap :: FT_GlyphSlot -> IO GlyphBitmap
readGlyphBitmap slot = do
    let bitmap = slot `plusPtr` (#offset FT_GlyphSlotRec, bitmap)
    rows <- (#peek FT_Bitmap, rows) bitmap :: IO CUInt
    width <- (#peek FT_Bitmap, width) bitmap :: IO CUInt
    pitch <- (#peek FT_Bitmap, pitch) bitmap :: IO CInt
    buffer <- (#peek FT_Bitmap, buffer) bitmap :: IO (Ptr Word8)
    left <- (#peek FT_GlyphSlotRec, bitmap_left) slot :: IO CInt
    top <- (#peek FT_GlyphSlotRec, bitmap_top) slot :: IO CInt
    advance <- (#peek FT_GlyphSlotRec, advance.x) slot :: IO CLong
    let w = fromIntegral width
        h = fromIntegral rows
        p = fromIntegral pitch
    img <- copyBitmap buffer w h p
    pure
        GlyphBitmap
            { gbImage = img
            , gbLeft = fromIntegral left
            , gbTop = fromIntegral top
            , -- FreeType advances are 26.6 fixed point; drop the fractional bits.
              gbAdvance = fromIntegral advance `shiftR` 6
            }

{- | Copy @h@ rows of @w@ grayscale bytes from @buf@ (row stride @pitch@) into a
tightly-packed 'Image' 'Pixel8'. Assumes a top-down 8-bit gray bitmap, which is
what @FT_LOAD_RENDER@ produces by default.
-}
copyBitmap :: Ptr Word8 -> Int -> Int -> Int -> IO (Image Pixel8)
copyBitmap buf w h pitch
    | w <= 0 || h <= 0 || buf == nullPtr =
        pure (Image (max 0 w) (max 0 h) (VS.replicate (max 0 w * max 0 h) 0))
    | otherwise = do
        mv <- VSM.new (w * h)
        let copyRow y =
                let srcRow = buf `plusPtr` (y * pitch)
                    copyPx x = do
                        b <- peekByteOff srcRow x :: IO Word8
                        VSM.write mv (y * w + x) b
                 in mapM_ copyPx [0 .. w - 1]
        mapM_ copyRow [0 .. h - 1]
        frozen <- VS.freeze mv
        pure (Image w h frozen)

checkFT :: String -> FT_Error -> IO ()
checkFT what err =
    when (err /= 0) $
        error (what <> " failed with FreeType error code " <> show err)
