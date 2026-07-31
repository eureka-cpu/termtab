module Termtab.UI.Widgets.Tablature (renderTablature, cursorAttr, playheadAttr, selectionAttr, barLineAttr, stringLabelAttr) where

import Brick hiding (zoom)
import Data.Map.Strict qualified as Map

import Termtab.Types
import Termtab.UI.Types

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

playheadAttr :: AttrName
playheadAttr = attrName "playhead"

selectionAttr :: AttrName
selectionAttr = attrName "selection"

barLineAttr :: AttrName
barLineAttr = attrName "barLine"

stringLabelAttr :: AttrName
stringLabelAttr = attrName "stringLabel"

renderTablature :: AppState -> TrackIndex -> Widget ResourceName
renderTablature st tIdx = Widget Greedy Fixed $ do
    ctx <- getContext
    let w = availWidth ctx
        track = getTrack tIdx (asSong st)
        nStrings = stringCountForTrack' track
        labels = stringLabels track
        labelWidth = maximum (map length labels) + 1 -- +1 for '|'
        contentWidth = w - labelWidth
        measures = songMeasures (asSong st)
        zoom = asZoom st
        -- Try to show from measure 0; scroll only if cursor isn't visible
        MeasureIndex cursorM = asCurrentMeasure st
        allFromZero = fitMeasures track measures zoom contentWidth 0
        cursorVisible = any (\(MeasureIndex mi, _, _) -> mi == cursorM) allFromZero
        visibleMeasures =
            if cursorVisible
                then allFromZero
                else fitMeasures track measures zoom contentWidth cursorM
        isFocused = tIdx == asCurrentTrack st
    render $
        vBox
            [ renderStringLine st tIdx track isFocused labels labelWidth zoom visibleMeasures (StringIndex si)
            | si <- reverse [0 .. nStrings - 1]
            ]

getTrack :: TrackIndex -> Song -> Track
getTrack (TrackIndex i) song =
    case drop i (songTracks song) of
        (t : _) -> t
        [] -> Track "?" (Standard (MidiProgram 0)) (MidiChannel 0) Map.empty

stringCountForTrack' :: Track -> Int
stringCountForTrack' track = case trackInstrument track of
    Guitar _ n -> n
    Bass _ n -> n
    Standard _ -> 1

-- | Tuning labels for each string, high to low
stringLabels :: Track -> [String]
stringLabels track = case trackInstrument track of
    Guitar tuning n -> map pitchLabel (take n tuning)
    Bass tuning n -> map pitchLabel (take n tuning)
    Standard _ -> [""]

pitchLabel :: Pitch -> String
pitchLabel (Pitch p) =
    let noteNames = ["C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"]
     in noteNames !! (p `mod` 12)

{- | Column width for a beat based on its duration relative to a quarter note.
At zoom level 4: quarter=4, eighth=2, sixteenth=1, half=8, whole=16, etc.
-}
beatColumnWidth :: Int -> Duration -> Int
beatColumnWidth zoom dur = max 1 (zoom * durationWeight dur `div` 4)

-- Quarter note = 4 units (the reference). Other durations scale from there.
durationWeight :: Duration -> Int
durationWeight Whole = 16
durationWeight Half = 8
durationWeight Quarter = 4
durationWeight Eighth = 2
durationWeight Sixteenth = 1
durationWeight Thirty2nd = 1
durationWeight (Dotted d) = durationWeight d + durationWeight d `div` 2
durationWeight (Triplet d) = durationWeight d * 2 `div` 3

-- | Fixed measure width from time signature. A 4/4 measure is always 4*zoom columns.
measureWidthFromTimeSig :: Int -> Measure -> Int
measureWidthFromTimeSig zoom m =
    let TimeSignature num _ = timeSignature m
     in num * zoom + 1 -- +1 for bar line

{- | Determine which measures fit in the available width.
Measure width is fixed by time signature; individual beat widths are proportional.
-}
fitMeasures :: Track -> [Measure] -> Int -> Int -> Int -> [(MeasureIndex, [Beat], Measure)]
fitMeasures track measures zoom availW startM = go startM availW
  where
    totalM = length measures
    go mi remaining
        | mi >= totalM = []
        | otherwise =
            let measure = measures !! mi
                beats = beatsForTrackMeasure track (MeasureIndex mi)
                mWidth = measureWidthFromTimeSig zoom measure
             in if mWidth > remaining && mi > startM
                    then []
                    else (MeasureIndex mi, beats, measure) : go (mi + 1) (remaining - mWidth)

renderStringLine ::
    AppState ->
    TrackIndex ->
    Track ->
    Bool ->
    [String] ->
    Int ->
    Int ->
    [(MeasureIndex, [Beat], Measure)] ->
    StringIndex ->
    Widget ResourceName
renderStringLine st _tIdx _track isFocused labels labelWidth zoom visibleMeasures sIdx =
    hBox $
        [ withAttr stringLabelAttr $ str (padLabel labelWidth (getLabel labels sIdx) ++ "|")
        ]
            ++ concatMap (renderMeasureOnString st isFocused zoom sIdx) visibleMeasures

padLabel :: Int -> String -> String
padLabel w s =
    let pad = w - length s - 1 -- -1 for the '|' added separately
     in replicate pad ' ' ++ s

getLabel :: [String] -> StringIndex -> String
getLabel labels (StringIndex si)
    | si < length labels = labels !! si
    | otherwise = ""

renderMeasureOnString ::
    AppState ->
    Bool ->
    Int ->
    StringIndex ->
    (MeasureIndex, [Beat], Measure) ->
    [Widget ResourceName]
renderMeasureOnString st isFocused zoom sIdx (mi, beats, measure) =
    let TimeSignature num _ = timeSignature measure
        -- Total columns for the measure (from time signature)
        totalCols = num * zoom
        -- Columns used by actual beats
        usedCols = sum (map (beatColumnWidth zoom . beatDuration) beats)
        -- Remaining columns filled with quarter-note-width empty slots
        remainingCols = max 0 (totalCols - usedCols)
        emptySlotWidth = zoom -- each empty slot is quarter-note width
        emptySlotCount = if emptySlotWidth > 0 then remainingCols `div` emptySlotWidth else 0
        -- Render actual beats
        beatWidgets =
            [ renderBeatCell st isFocused zoom sIdx mi (BeatIndex bi) beats
            | bi <- [0 .. length beats - 1]
            ]
        -- Render empty beat slots
        emptyWidgets =
            [ renderEmptyCell st isFocused sIdx mi (BeatIndex bi) emptySlotWidth
            | bi <- [length beats .. length beats + emptySlotCount - 1]
            ]
        barLine = withAttr barLineAttr (str "|")
     in beatWidgets ++ emptyWidgets ++ [barLine]

renderEmptyCell ::
    AppState ->
    Bool ->
    StringIndex ->
    MeasureIndex ->
    BeatIndex ->
    Int ->
    Widget ResourceName
renderEmptyCell st isFocused sIdx mi bi colWidth =
    let cell = replicate (max 1 colWidth) '-'
        isCursor =
            isFocused
                && mi == asCurrentMeasure st
                && bi == asCurrentBeat st
                && sIdx == asCurrentString st
        attr
            | isCursor = cursorAttr
            | otherwise = mempty
     in withAttr attr (str cell)

renderBeatCell ::
    AppState ->
    Bool ->
    Int ->
    StringIndex ->
    MeasureIndex ->
    BeatIndex ->
    [Beat] ->
    Widget ResourceName
renderBeatCell st isFocused zoom sIdx mi bi beats =
    let BeatIndex bIdx = bi
        mBeat = case drop bIdx beats of
            (beat : _) -> Just beat
            [] -> Nothing
        colWidth = case mBeat of
            Just beat -> beatColumnWidth zoom (beatDuration beat)
            Nothing -> zoom
        fretText = case mBeat of
            Just beat -> fretOnString sIdx beat
            Nothing -> Nothing
        cell = case fretText of
            Just f -> padCell colWidth (show f)
            Nothing -> replicate colWidth '-'
        isCursor =
            isFocused
                && mi == asCurrentMeasure st
                && bi == asCurrentBeat st
                && sIdx == asCurrentString st
        isPlayhead =
            asPlayheadMeasure st == Just mi
                && asPlayheadBeat st == Just bi
        isSelected = case asSelectionStart st of
            Just (selMi, selBi) ->
                mi == selMi
                    && mi == asCurrentMeasure st
                    && let BeatIndex s = min selBi (asCurrentBeat st)
                           BeatIndex e = max selBi (asCurrentBeat st)
                           BeatIndex b = bi
                        in b >= s && b <= e
            Nothing -> False
        attr
            | isCursor = cursorAttr
            | isSelected = selectionAttr
            | isPlayhead = playheadAttr
            | otherwise = mempty
     in withAttr attr (str cell)

-- | Find the fret number for a given string in a beat
fretOnString :: StringIndex -> Beat -> Maybe Int
fretOnString sIdx beat =
    case filter (matchesString sIdx) (beatNotes beat) of
        (n : _) -> case noteFret n of
            Just (FretNumber f) -> Just f
            Nothing -> Nothing
        [] -> Nothing

matchesString :: StringIndex -> Note -> Bool
matchesString sIdx n = noteString n == Just sIdx

padCell :: Int -> String -> String
padCell w s
    | length s >= w = take w s
    | otherwise = s ++ replicate (w - length s) '-'
