module Termtab.UI.Widgets.Notation (renderNotation) where

import Brick hiding (zoom)
import Data.Map.Strict qualified as Map

import Termtab.Types
import Termtab.UI.Types
import Termtab.UI.Widgets.Tablature (beatColumnWidth, findVisibleRange)

{- | Render standard notation above the tab.
9 rows: 5 staff lines (even positions) + 4 spaces (odd positions).
Staff positions: 0=E4, 1=F4, 2=G4, 3=A4, 4=B4, 5=C5, 6=D5, 7=E5, 8=F5
Lines are at positions 0, 2, 4, 6, 8. Spaces at 1, 3, 5, 7.
-}
renderNotation :: AppState -> TrackIndex -> Widget ResourceName
renderNotation st tIdx =
    let track = getTrack tIdx (asSong st)
        measures = songMeasures (asSong st)
        zoom = asZoom st
        MeasureIndex cursorM = asCurrentMeasure st
        visibleMeasures = findVisibleRange track measures zoom 200 cursorM
        instr = trackInstrument track
     in vBox $
            -- Above staff (ledger lines / high notes)
            [renderStaffRow zoom instr visibleMeasures 10 False]
                ++ [renderStaffRow zoom instr visibleMeasures 9 False]
                -- Staff: top line (F5) down to bottom line (E4)
                -- Alternating: line, space, line, space, ...
                ++ [ renderStaffRow zoom instr visibleMeasures pos (isLine pos)
                   | pos <- [8, 7, 6, 5, 4, 3, 2, 1, 0]
                   ]
                -- Below staff (ledger lines / low notes)
                ++ [renderStaffRow zoom instr visibleMeasures (-1) False]
                ++ [renderStaffRow zoom instr visibleMeasures (-2) False]

isLine :: Int -> Bool
isLine pos = pos `elem` [0, 2, 4, 6, 8]

getTrack :: TrackIndex -> Song -> Track
getTrack (TrackIndex i) song =
    case drop i (songTracks song) of
        (t : _) -> t
        [] -> Track "?" (Standard (MidiProgram 0)) (MidiChannel 0) Map.empty

-- | Render one row of the staff (either a line or a space).
renderStaffRow :: Int -> Instrument -> [(MeasureIndex, [Beat], Measure)] -> Int -> Bool -> Widget ResourceName
renderStaffRow zoom instr visibleMeasures staffPos onLine =
    let fillChar = if onLine then '─' else ' '
        barChar = if staffPos >= 0 && staffPos <= 8 then "│" else " "
     in hBox $
            [str [fillChar]] -- label column
                ++ concatMap (renderMeasureAtPos zoom instr staffPos fillChar barChar) visibleMeasures

renderMeasureAtPos :: Int -> Instrument -> Int -> Char -> String -> (MeasureIndex, [Beat], Measure) -> [Widget ResourceName]
renderMeasureAtPos zoom instr staffPos fillChar barChar (_mi, beats, measure) =
    let TimeSignature num _ = timeSignature measure
        totalCols = num * zoom
        usedCols = sum (map (beatColumnWidth zoom . beatDuration) beats)
        remainingCols = max 0 (totalCols - usedCols)
        emptySlotWidth = max 3 zoom
        emptySlotCount = if emptySlotWidth > 0 then remainingCols `div` emptySlotWidth else 0
        beatWidgets =
            [ renderBeatAtPos zoom instr staffPos fillChar (BeatIndex bi) beats
            | bi <- [0 .. length beats - 1]
            ]
        emptyWidgets = [str (replicate (emptySlotCount * emptySlotWidth) fillChar)]
        barLine = str barChar
     in beatWidgets ++ emptyWidgets ++ [barLine]

renderBeatAtPos :: Int -> Instrument -> Int -> Char -> BeatIndex -> [Beat] -> Widget ResourceName
renderBeatAtPos zoom instr staffPos fillChar (BeatIndex bIdx) beats =
    let mBeat = case drop bIdx beats of
            (b : _) -> Just b
            [] -> Nothing
        colWidth = case mBeat of
            Just b -> beatColumnWidth zoom (beatDuration b)
            Nothing -> max 3 zoom
     in case mBeat of
            Nothing -> str (replicate colWidth fillChar)
            Just beat
                | beatIsRest beat && staffPos == 4 ->
                    -- Rest at middle of staff
                    str (placeGlyph colWidth fillChar '𝄾')
                | beatIsRest beat ->
                    str (replicate colWidth fillChar)
                | otherwise ->
                    let matching = filter (noteOnStaffPos instr staffPos) (beatNotes beat)
                     in if null matching
                            then str (replicate colWidth fillChar)
                            else
                                let nh = noteheadChar (beatDuration beat)
                                 in str (placeGlyph colWidth fillChar nh)

-- | Notehead character based on duration.
noteheadChar :: Duration -> Char
noteheadChar Whole = '\xE0A2' -- SMuFL whole notehead
noteheadChar Half = '\xE0A3' -- SMuFL half notehead
noteheadChar (Dotted d) = noteheadChar d
noteheadChar (Triplet d) = noteheadChar d
noteheadChar _ = '\xE0A4' -- SMuFL filled (black) notehead

noteOnStaffPos :: Instrument -> Int -> Note -> Bool
noteOnStaffPos instr targetPos note =
    pitchToStaffPos instr (notePitch note) == targetPos

{- | Convert MIDI pitch to staff position in treble clef.
Guitar/bass transposed up one octave (standard convention).
Position 0 = E4 (bottom line), 1 = F4, 2 = G4, ..., 8 = F5 (top line)
-}
pitchToStaffPos :: Instrument -> Pitch -> Int
pitchToStaffPos instr (Pitch midi) =
    let transposed = case instr of
            Guitar _ _ -> midi + 12
            Bass _ _ -> midi + 12
            Standard _ -> midi
        octave = transposed `div` 12 - 1
        noteInOctave = transposed `mod` 12
        naturalPos = case noteInOctave of
            0 -> 0
            1 -> 0
            2 -> 1
            3 -> 1
            4 -> 2
            5 -> 3
            6 -> 3
            7 -> 4
            8 -> 4
            9 -> 5
            10 -> 5
            11 -> 6
            _ -> 0
        posFromC4 = (octave - 4) * 7 + naturalPos
     in posFromC4 - 2

-- | Place a glyph character centered in a cell.
placeGlyph :: Int -> Char -> Char -> String
placeGlyph w fillChar glyph
    | w <= 1 = [glyph]
    | otherwise =
        let remaining = w - 1
            lPad = remaining `div` 2
            rPad = remaining - lPad
         in replicate lPad fillChar ++ [glyph] ++ replicate rPad fillChar
