{- | One-shot notation demos used to validate the graphics pipeline in a real
terminal (Phase 0), independent of the TUI.

* 'runNotationDemo' uses direct placement (@a=T@) to prove the Kitty encoder,
  renderer, and foreground query work end to end.
* 'runPlaceholderDemo' uses a virtual placement plus Unicode placeholder cells —
  the same mechanism the brick widget will use — to prove images can be shown
  through ordinary terminal cells.
-}
module Termtab.Graphics.Demo (
    runNotationDemo,
    runPlaceholderDemo,
) where

import Codec.Picture (Image, PixelRGBA8)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import System.IO (hFlush, hPutStrLn, stderr, stdout)

import Termtab.Defaults (defaultSong)
import Termtab.Graphics.Font
import Termtab.Graphics.Kitty (encodeImage, encodeVirtual)
import Termtab.Graphics.KittyPlacement
import Termtab.Graphics.Notation (defaultLayout, renderNotationImage)
import Termtab.Graphics.TermColor (foregroundOrDefault)
import Termtab.Types

-- | Direct placement: draw the notation image at the cursor.
runNotationDemo :: IO ()
runNotationDemo = withDefaultNotation $ \img -> do
    BS.hPut stdout (encodeImage img)
    hFlush stdout
    putStr (replicate 9 '\n')
    putStrLn "^ termtab notation (Kitty graphics protocol, foreground-colored)"

-- | Virtual placement shown through Unicode placeholder cells.
runPlaceholderDemo :: IO ()
runPlaceholderDemo = withDefaultNotation $ \img -> do
    let imgId = 1
        cols = 24
        rows = 7
    BS.hPut stdout (encodeVirtual imgId cols rows img)
    hFlush stdout
    let sgr = foregroundIdSGR imgId
    forM_ (placeholderRows cols rows) $ \line ->
        putStrLn (sgr <> line <> "\ESC[0m")
    putStrLn "^ termtab notation (Kitty Unicode placeholders)"

{- | Render the default scratch measure with foreground-colored ink and hand it
to a display action, or report why it can't.
-}
withDefaultNotation :: (Image PixelRGBA8 -> IO ()) -> IO ()
withDefaultNotation display = do
    mFont <- bravuraFontPath
    case mFont of
        Nothing ->
            hPutStrLn stderr "TERMTAB_BRAVURA_FONT is not set (use the Nix devshell)."
        Just path -> do
            ink <- foregroundOrDefault
            withGlyphFont path $ \font -> do
                let song = defaultSong
                case (songTracks song, songMeasures song) of
                    (track : _, measure : _) -> do
                        let instr = trackInstrument track
                            beats = Map.findWithDefault [] (MeasureIndex 0) (trackBeats track)
                            measures = [(MeasureIndex 0, beats, measure)]
                        img <- renderNotationImage font defaultLayout ink 4 instr measures
                        display img
                    _ -> hPutStrLn stderr "default song is missing a track or measure"
