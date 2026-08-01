{- | Rasterized standard-notation renderer.

Composes the visible measures of a track into a single RGBA image: a five-line
staff, a treble clef, bar lines, and Bravura noteheads/rests placed at their
staff positions. This is the image that the Kitty/Sixel backends blit above the
tab grid.

Beat x-positions are derived from the same @beatColumnWidth@ column math the tab
uses, so notation and tab stay column-aligned. The mapping from columns to
pixels ('nlColWidthPx') is a layout parameter tuned against the terminal's real
cell size when the renderer is wired into the UI (Phase 0).

Note: this imports 'beatColumnWidth' from the tab widget for now; that column
helper is a candidate for extraction into a shared layout module later.
-}
module Termtab.Graphics.Notation (
    NotationLayout (..),
    defaultLayout,
    renderNotationImage,
    totalColumns,
) where

import Codec.Picture
import Codec.Picture.Types (MutableImage (..), createMutableImage, readPixel, unsafeFreezeImage, writePixel)
import Control.Monad (forM_)
import Control.Monad.ST (RealWorld)
import Data.Word (Word8)

import Termtab.Graphics.Font
import Termtab.Graphics.FreeType (GlyphBitmap (..))
import Termtab.Notation.Staff
import Termtab.Types
import Termtab.UI.Widgets.Tablature (beatColumnWidth)

-- | Pixel geometry for the notation image.
data NotationLayout = NotationLayout
    { nlStepPx :: Int
    -- ^ Vertical pixels between adjacent staff positions (line <-> space).
    , nlColWidthPx :: Int
    -- ^ Pixels per tab column (governs horizontal scale + tab alignment).
    , nlLabelCols :: Int
    -- ^ Columns reserved at the left for the clef (matches the tab's label column).
    , nlLedgerAbove :: Int
    -- ^ Extra staff positions of headroom above the top line.
    , nlLedgerBelow :: Int
    -- ^ Extra staff positions of headroom below the bottom line.
    , nlMarginPx :: Int
    -- ^ Top/bottom padding in pixels.
    }
    deriving (Show, Eq)

defaultLayout :: NotationLayout
defaultLayout =
    NotationLayout
        { nlStepPx = 6
        , nlColWidthPx = 10
        , nlLabelCols = 1
        , nlLedgerAbove = 4
        , nlLedgerBelow = 4
        , nlMarginPx = 4
        }

type Canvas = MutableImage RealWorld PixelRGBA8

{- | Render the visible measures of a track to an RGBA notation image.

The background is fully transparent and all ink (staff, clef, bar lines,
noteheads) is drawn in @ink@ — pass the terminal's foreground color so the
notation adopts the user's theme (see "Termtab.Graphics.TermColor"). Glyph
anti-aliasing is preserved via the alpha channel.
-}
renderNotationImage ::
    GlyphFont ->
    NotationLayout ->
    PixelRGBA8 ->
    Int ->
    Instrument ->
    [(MeasureIndex, [Beat], Measure)] ->
    IO (Image PixelRGBA8)
renderNotationImage font nl ink zoom instr measures = do
    let totalCols = totalColumns nl zoom measures
        width = max 1 (totalCols * nlColWidthPx nl)
        topPos = 8 + nlLedgerAbove nl
        bottomPos = negate (nlLedgerBelow nl)
        height = nlMarginPx nl * 2 + (topPos - bottomPos) * nlStepPx nl + 1
        glyphSize = 8 * nlStepPx nl
        yOf pos = nlMarginPx nl + (topPos - pos) * nlStepPx nl
        xOf col = round (col * fromIntegral (nlColWidthPx nl) :: Double)
        lineThick = max 1 (nlStepPx nl `div` 4)
        barThick = max 1 (nlStepPx nl `div` 3)
    -- Transparent background: the terminal's own background shows through.
    canvas <- createMutableImage width height (PixelRGBA8 0 0 0 0)

    -- Five staff lines across the full width.
    forM_ [0, 2, 4, 6, 8] $ \pos ->
        drawHLine canvas ink 0 (width - 1) (yOf pos) lineThick

    -- Treble clef at the left, baseline on the G4 line (position 2).
    mClef <- cachedGlyph font glyphSize gClefGlyph
    forM_ mClef $ \gb -> drawGlyphAt canvas ink (nlColWidthPx nl `div` 2) (yOf 2) gb

    let Placed placedBeats barCols = layoutColumns nl zoom measures

    -- Bar lines run from the top line to the bottom line.
    forM_ barCols $ \bc ->
        drawVLine canvas ink (xOf (fromIntegral bc)) (yOf 8) (yOf 0) barThick

    -- Beats: rests near the middle line, noteheads at their staff positions.
    forM_ placedBeats $ \(centerCol, beat) -> do
        let cx = xOf centerCol
        if beatIsRest beat
            then do
                mg <- cachedGlyph font glyphSize (restGlyph (beatDuration beat))
                forM_ mg $ \gb -> drawGlyphCentered canvas ink cx (yOf 4) gb
            else forM_ (beatNotes beat) $ \note -> do
                let pos = pitchToStaffPos instr (notePitch note)
                mg <- cachedGlyph font glyphSize (noteheadGlyph (beatDuration beat))
                forM_ mg $ \gb -> do
                    drawGlyphCentered canvas ink cx (yOf pos) gb
                    drawLedgers canvas nl ink yOf cx width pos

    unsafeFreezeImage canvas

-- Column layout -------------------------------------------------------------

data Placed = Placed
    { pBeats :: [(Double, Beat)]
    , pBarCols :: [Int]
    }

{- | Width in columns of one measure's content (excluding its bar line),
matching the tab's accounting: beats consume 'beatColumnWidth' each, then empty
quarter slots pad out any remaining nominal width. Subdivided measures whose
beats exceed the nominal width expand to fit (as the tab does).
-}
measureColumns :: Int -> [Beat] -> Measure -> Int
measureColumns zoom beats measure =
    let TimeSignature num _ = timeSignature measure
        nominal = num * zoom
        used = sum (map (beatColumnWidth zoom . beatDuration) beats)
        remaining = max 0 (nominal - used)
        emptySlots = if zoom > 0 then remaining `div` zoom else 0
     in used + emptySlots * zoom

-- | Total tab columns across all visible measures (label + measures + bar lines).
totalColumns :: NotationLayout -> Int -> [(MeasureIndex, [Beat], Measure)] -> Int
totalColumns nl zoom measures =
    nlLabelCols nl
        + sum [measureColumns zoom beats m + 1 | (_, beats, m) <- measures]

{- | Walk the measures assigning each beat a (fractional) center column and each
measure a bar-line column, mirroring the tab's column accounting.
-}
layoutColumns :: NotationLayout -> Int -> [(MeasureIndex, [Beat], Measure)] -> Placed
layoutColumns nl zoom = go (fromIntegral (nlLabelCols nl))
  where
    go _ [] = Placed [] []
    go startCol ((_, beats, m) : rest) =
        let mw = measureColumns zoom beats m
            placedBeats = placeBeats startCol beats
            barCol = round startCol + mw
            Placed bs bars = go (startCol + fromIntegral mw + 1) rest
         in Placed (placedBeats ++ bs) (barCol : bars)
    placeBeats _ [] = []
    placeBeats c (b : bs) =
        let w = beatColumnWidth zoom (beatDuration b)
            center = c + fromIntegral w / 2
         in (center, b) : placeBeats (c + fromIntegral w) bs

-- Drawing primitives --------------------------------------------------------

-- | Short ledger lines for a note above/below the staff.
drawLedgers :: Canvas -> NotationLayout -> PixelRGBA8 -> (Int -> Int) -> Int -> Int -> Int -> IO ()
drawLedgers canvas nl ink yOf cx _width pos =
    forM_ positions $ \p ->
        drawHLine canvas ink (cx - half) (cx + half) (yOf p) (max 1 (nlStepPx nl `div` 4))
  where
    half = nlColWidthPx nl
    positions
        | pos > 8 = [p | p <- [10, 12 .. pos]]
        | pos < 0 = [p | p <- [-2, -4 .. pos]]
        | otherwise = []

drawHLine :: Canvas -> PixelRGBA8 -> Int -> Int -> Int -> Int -> IO ()
drawHLine canvas ink x0 x1 y thick =
    forM_ [0 .. thick - 1] $ \t ->
        forM_ [x0 .. x1] $ \x -> setInk canvas ink x (y + t)

drawVLine :: Canvas -> PixelRGBA8 -> Int -> Int -> Int -> Int -> IO ()
drawVLine canvas ink x y0 y1 thick =
    forM_ [0 .. thick - 1] $ \t ->
        forM_ [y0 .. y1] $ \y -> setInk canvas ink (x + t) y

-- | Draw a glyph with its pen origin at (penX, baselineY).
drawGlyphAt :: Canvas -> PixelRGBA8 -> Int -> Int -> GlyphBitmap -> IO ()
drawGlyphAt canvas ink penX baselineY gb = do
    let img = gbImage gb
        gw = imageWidth img
        gh = imageHeight img
        originX = penX + gbLeft gb
        originY = baselineY - gbTop gb
    forM_ [0 .. gh - 1] $ \gy ->
        forM_ [0 .. gw - 1] $ \gx ->
            blendInk canvas ink (originX + gx) (originY + gy) (pixelAt img gx gy)

-- | Draw a glyph horizontally centered on centerX, baseline at baselineY.
drawGlyphCentered :: Canvas -> PixelRGBA8 -> Int -> Int -> GlyphBitmap -> IO ()
drawGlyphCentered canvas ink centerX baselineY gb =
    drawGlyphAt canvas ink (centerX - gbAdvance gb `div` 2) baselineY gb

-- | Composite fully-opaque ink (used for staff/stems/bar lines).
setInk :: Canvas -> PixelRGBA8 -> Int -> Int -> IO ()
setInk canvas ink x y = blendInk canvas ink x y 255

{- | Composite @ink@ with the given coverage as alpha, keeping the strongest
coverage where marks overlap (all ink is one color, so max-alpha is correct).
-}
blendInk :: Canvas -> PixelRGBA8 -> Int -> Int -> Word8 -> IO ()
blendInk canvas (PixelRGBA8 r g b _) x y cov
    | cov == 0 = pure ()
    | x < 0 || y < 0 || x >= mutableImageWidth canvas || y >= mutableImageHeight canvas = pure ()
    | otherwise = do
        PixelRGBA8 _ _ _ a <- readPixel canvas x y
        writePixel canvas x y (PixelRGBA8 r g b (max a cov))
