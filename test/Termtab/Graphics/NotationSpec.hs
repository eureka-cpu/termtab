module Termtab.Graphics.NotationSpec (tests) where

import Codec.Picture (PixelRGBA8 (..), imageData, imageHeight, imageWidth, writePng)
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map
import Data.Vector.Storable qualified as VS
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Defaults (defaultSong)
import Termtab.Graphics.Font
import Termtab.Graphics.Notation
import Termtab.Types

{- | Renders the default scratch measure to an image and checks it is non-empty.
Skipped when @TERMTAB_BRAVURA_FONT@ is unset. If @TERMTAB_NOTATION_PNG@ is set,
also dumps the image there for visual inspection.
-}
tests :: TestTree
tests =
    testGroup
        "Graphics.Notation"
        [ testCase "renders the default measure with staff + noteheads" $ do
            mFont <- lookupEnv "TERMTAB_BRAVURA_FONT"
            case mFont of
                Nothing -> pure ()
                Just path -> withGlyphFont path $ \font -> do
                    let song = defaultSong
                    case (songTracks song, songMeasures song) of
                        (track : _, measure : _) -> do
                            let instr = trackInstrument track
                                beats = Map.findWithDefault [] (MeasureIndex 0) (trackBeats track)
                                measures = [(MeasureIndex 0, beats, measure)]
                            -- Black ink on the transparent canvas so the dumped
                            -- PNG is legible in an image viewer.
                            let ink = PixelRGBA8 0 0 0 255
                            img <- renderNotationImage font defaultLayout ink 4 instr measures
                            assertBool "positive width" (imageWidth img > 0)
                            assertBool "positive height" (imageHeight img > 0)
                            assertBool "image has ink" (VS.any (/= 0) (imageData img))
                            mOut <- lookupEnv "TERMTAB_NOTATION_PNG"
                            forM_ mOut $ \out -> writePng out img
                        _ -> assertFailure "default song is missing a track or measure"
        ]
