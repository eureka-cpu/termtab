{- | Glue between the notation image renderer and the brick UI, using Kitty
graphics protocol direct placement.

Each track that shows notation reserves a fixed-height region ('notationRegionWidget')
of blank cells. brick lays it out and reports its on-screen extent; in the event
handler 'syncNotationGraphics' looks that extent up, renders the notation image,
moves the cursor there, and blits the image scaled to the region (Kitty draws it
over the blank cells). Re-emitting with a stable image/placement id replaces the
placement rather than stacking.

Gated on the detected protocol being 'Kitty' with Bravura loaded; otherwise the
caller falls back to the Unicode text staff.
-}
module Termtab.UI.NotationGraphics (
    syncNotationGraphics,
    notationRegionWidget,
    graphicsNotationActive,
    notationRows,
) where

import Brick
import Brick.Main (lookupExtent)
import Brick.Types (locationColumnL, locationRowL)
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Map.Strict qualified as Map
import Lens.Micro ((^.))
import System.IO (hFlush, stdout)

import Termtab.Graphics.Detect (GraphicsProtocol (..))
import Termtab.Graphics.Font (GlyphFont)
import Termtab.Graphics.Kitty (deleteImage, encodeDirect)
import Termtab.Graphics.Notation (defaultLayout, renderNotationImage, totalColumns)
import Termtab.Types
import Termtab.UI.Types
import Termtab.UI.Widgets.Tablature (findVisibleRange)

-- | Height of the notation strip, in terminal cells.
notationRows :: Int
notationRows = 7

-- | Kitty image id for a track's notation (distinct per track, nonzero).
trackImageId :: TrackIndex -> Int
trackImageId (TrackIndex i) = i + 1

{- | True when the graphics notation path is available: the terminal speaks the
Kitty protocol and Bravura loaded.
-}
graphicsNotationActive :: AppState -> Bool
graphicsNotationActive st =
    asProtocol st == Kitty && maybe False (const True) (asGlyphFont st)

{- | The reserved region a track's notation image is blitted over: blank cells
whose extent we report so the transmit step can find its screen position.
-}
notationRegionWidget :: TrackIndex -> Widget ResourceName
notationRegionWidget tIdx =
    reportExtent (NotationRegion tIdx) (vLimit notationRows (fill ' '))

{- | For every track, blit its notation image if it shows notation and its
content/position changed since last time, or clear a stale image if it stopped
showing notation. A no-op when graphics are unavailable.
-}
syncNotationGraphics :: EventM ResourceName AppState ()
syncNotationGraphics = do
    st <- get
    case (asProtocol st, asGlyphFont st) of
        (Kitty, Just font) ->
            forM_ (zip [0 ..] (songTracks (asSong st))) $ \(i, track) -> do
                let tIdx = TrackIndex i
                if showsNotation (trackDisplayMode st tIdx track)
                    then do
                        mExt <- lookupExtent (NotationRegion tIdx)
                        forM_ mExt $ emitTrack st font tIdx track
                    else clearTrackImage tIdx
        _ -> pure ()

{- | Render one track's notation and blit it over its reserved extent — but only
if its signature changed, so in-place navigation doesn't re-blit (and flicker).
-}
emitTrack ::
    AppState ->
    GlyphFont ->
    TrackIndex ->
    Track ->
    Extent ResourceName ->
    EventM ResourceName AppState ()
emitTrack st font tIdx track ext = do
    let ul = extentUpperLeft ext
        col = ul ^. locationColumnL
        row = ul ^. locationRowL
        (w, _h) = extentSize ext
        zoom = asZoom st
        MeasureIndex cursorM = asCurrentMeasure st
        -- Match the tab: it reserves one label column, then the measures.
        contentWidth = max 1 (w - 1)
        visible = findVisibleRange track (songMeasures (asSong st)) zoom contentWidth cursorM
        -- Blit only as many cells wide as the content, so one layout column maps
        -- to one terminal cell and the notation lines up with the tab below.
        cols = totalColumns defaultLayout zoom visible
        sig = notationSignature col row cols visible
        imgId = trackImageId tIdx
    prev <- gets (Map.lookup tIdx . asNotationSigs)
    when (prev /= Just sig) $ do
        img <-
            liftIO $
                renderNotationImage font defaultLayout (asInkColor st) zoom (trackInstrument track) visible
        -- Delete the previous image, move the cursor to the region's top-left
        -- (1-based), then blit scaled to cols×notationRows cells. Delete + place
        -- go out in one write so the terminal never repaints between them.
        let move = BC.pack ("\ESC[" <> show (row + 1) <> ";" <> show (col + 1) <> "H")
            payload = deleteImage imgId <> move <> encodeDirect imgId cols notationRows img
        liftIO $ do
            BS.hPut stdout payload
            hFlush stdout
        modify $ \s -> s{asNotationSigs = Map.insert tIdx sig (asNotationSigs s)}

{- | Remove a track's on-screen image if one was placed (e.g. it switched to a
mode without notation).
-}
clearTrackImage :: TrackIndex -> EventM ResourceName AppState ()
clearTrackImage tIdx = do
    placed <- gets (Map.member tIdx . asNotationSigs)
    when placed $ do
        liftIO $ do
            BS.hPut stdout (deleteImage (trackImageId tIdx))
            hFlush stdout
        modify $ \s -> s{asNotationSigs = Map.delete tIdx (asNotationSigs s)}

{- | Cheap key of everything that determines the blitted image + its placement:
position, width, and the visible beats (durations, rests, pitches).
-}
notationSignature :: Int -> Int -> Int -> [(MeasureIndex, [Beat], Measure)] -> String
notationSignature col row cols visible =
    show (col, row, cols, map summariseMeasure visible)
  where
    summariseMeasure (MeasureIndex mi, beats, _) = (mi, map summariseBeat beats)
    summariseBeat b =
        (show (beatDuration b), beatIsRest b, [p | note <- beatNotes b, let Pitch p = notePitch note])

-- Display-mode helpers (mirroring Termtab.UI.Widgets.TrackPanel) -------------

trackDisplayMode :: AppState -> TrackIndex -> Track -> DisplayMode
trackDisplayMode st tIdx track =
    case trackInstrument track of
        Standard _ -> NotationOnly
        _ -> Map.findWithDefault TabOnly tIdx (asDisplayModes st)

showsNotation :: DisplayMode -> Bool
showsNotation TabOnly = False
showsNotation NotationOnly = True
showsNotation TabAndNotation = True
