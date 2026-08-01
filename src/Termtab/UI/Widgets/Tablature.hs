module Termtab.UI.Widgets.Tablature (renderTablature, cursorAttr, playheadAttr, selectionAttr, barLineAttr, stringLabelAttr, findVisibleRange, beatColumnWidth) where

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

-- | Unicode line character for string lines
line :: Char
line = '─'

-- | Unicode vertical bar for measure boundaries
bar :: String
bar = "│"

renderTablature :: AppState -> TrackIndex -> Widget ResourceName
renderTablature st tIdx = Widget Greedy Fixed $ do
    ctx <- getContext
    let w = availWidth ctx
        track = getTrack tIdx (asSong st)
        nStrings = stringCountForTrack' track
        labelWidth = 1 -- single character: T, A, B, or ─
        contentWidth = w - labelWidth
        measures = songMeasures (asSong st)
        zoom = asZoom st
        MeasureIndex cursorM = asCurrentMeasure st
        -- Find the best starting measure: latest start that still includes the cursor
        visibleMeasures = findVisibleRange track measures zoom contentWidth cursorM
        isFocused = tIdx == asCurrentTrack st
        labels = tabLabels nStrings
    render $
        vBox $
            [renderMeasureNumberRow labelWidth zoom visibleMeasures]
                ++ [ renderStringLine st isFocused labelWidth zoom visibleMeasures (StringIndex si) (labels !! si)
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

{- | GP-style labels: "T", "A", "B" on top 3 strings (which are the highest-pitched,
rendered first since we reverse), rest get line character.
-}
tabLabels :: Int -> [String]
tabLabels n
    | n >= 3 =
        replicate (n - 3) [line] ++ ["B", "A", "T"]
    | otherwise = replicate n [line]

-- | Render measure numbers above the tab
renderMeasureNumberRow :: Int -> Int -> [(MeasureIndex, [Beat], Measure)] -> Widget ResourceName
renderMeasureNumberRow labelWidth zoom visibleMeasures =
    hBox $
        [str (replicate labelWidth ' ')]
            ++ map (renderMeasureNumber zoom) visibleMeasures

renderMeasureNumber :: Int -> (MeasureIndex, [Beat], Measure) -> Widget ResourceName
renderMeasureNumber zoom (MeasureIndex mi, beats, measure) =
    let TimeSignature num _ = timeSignature measure
        totalCols = num * zoom
        usedCols = sum (map (beatColumnWidth zoom . beatDuration) beats)
        remainingCols = max 0 (totalCols - usedCols)
        emptySlotCount = if zoom > 0 then remainingCols `div` zoom else 0
        actualTotalCols = usedCols + emptySlotCount * zoom
        mNum = show (mi + 1)
        -- +1 for the bar line character
        padded = mNum ++ replicate (max 0 (actualTotalCols + 1 - length mNum)) ' '
     in str padded

-- | Column width for a beat, minimum 3 to fit double-digit frets
beatColumnWidth :: Int -> Duration -> Int
beatColumnWidth zoom dur = max 3 (zoom * durationWeight dur `div` 4)

durationWeight :: Duration -> Int
durationWeight Whole = 16
durationWeight Half = 8
durationWeight Quarter = 4
durationWeight Eighth = 2
durationWeight Sixteenth = 1
durationWeight Thirty2nd = 1
durationWeight (Dotted d) = durationWeight d + durationWeight d `div` 2
durationWeight (Triplet d) = durationWeight d * 2 `div` 3

measureWidthFromTimeSig :: Int -> Measure -> Int
measureWidthFromTimeSig zoom m =
    let TimeSignature num _ = timeSignature m
     in num * zoom + 1

{- | Find the best visible range: start as early as possible while
ensuring the cursor measure is visible and the width is filled.
-}
findVisibleRange :: Track -> [Measure] -> Int -> Int -> Int -> [(MeasureIndex, [Beat], Measure)]
findVisibleRange track measures zoom availW cursorM =
    -- Try each possible start from 0 to cursorM, pick the latest one
    -- that still includes the cursor measure
    let candidates =
            [ (startM, fitted)
            | startM <- [0 .. cursorM]
            , let fitted = fitMeasures track measures zoom availW startM
            , any (\(MeasureIndex mi, _, _) -> mi == cursorM) fitted
            ]
     in case candidates of
            [] -> fitMeasures track measures zoom availW cursorM
            ((_, fitted) : _) -> fitted

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
    Bool ->
    Int ->
    Int ->
    [(MeasureIndex, [Beat], Measure)] ->
    StringIndex ->
    String ->
    Widget ResourceName
renderStringLine st isFocused _labelWidth zoom visibleMeasures sIdx label =
    hBox $
        [withAttr stringLabelAttr (str label)]
            ++ concatMap (renderMeasureOnString st isFocused zoom sIdx) visibleMeasures

renderMeasureOnString ::
    AppState ->
    Bool ->
    Int ->
    StringIndex ->
    (MeasureIndex, [Beat], Measure) ->
    [Widget ResourceName]
renderMeasureOnString st isFocused zoom sIdx (mi, beats, measure) =
    let TimeSignature num _ = timeSignature measure
        totalCols = num * zoom
        usedCols = sum (map (beatColumnWidth zoom . beatDuration) beats)
        remainingCols = max 0 (totalCols - usedCols)
        emptySlotWidth = max 3 zoom
        emptySlotCount = if emptySlotWidth > 0 then remainingCols `div` emptySlotWidth else 0
        beatWidgets =
            [ renderBeatCell st isFocused zoom sIdx mi (BeatIndex bi) beats
            | bi <- [0 .. length beats - 1]
            ]
        emptyWidgets =
            [ renderEmptyCell st isFocused sIdx mi (BeatIndex bi) emptySlotWidth
            | bi <- [length beats .. length beats + emptySlotCount - 1]
            ]
        barLine = withAttr barLineAttr (str bar)
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
    let cell = replicate (max 1 colWidth) line
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
            Nothing -> max 3 zoom
        fretText = case mBeat of
            Just beat -> fretOnString sIdx beat
            Nothing -> Nothing
        cell = case fretText of
            Just f -> fretOnLine colWidth (show f)
            Nothing -> replicate colWidth line
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

-- | Place a fret number on a line: ─5── or 12── (number replaces line segments)
fretOnLine :: Int -> String -> String
fretOnLine w s
    | len >= w = take w s
    | otherwise =
        let remaining = w - len
            padLeft = remaining `div` 2
            padRight = remaining - padLeft
         in replicate padLeft line ++ s ++ replicate padRight line
  where
    len = length s

fretOnString :: StringIndex -> Beat -> Maybe Int
fretOnString sIdx beat =
    case filter (matchesString sIdx) (beatNotes beat) of
        (n : _) -> case noteFret n of
            Just (FretNumber f) -> Just f
            Nothing -> Nothing
        [] -> Nothing

matchesString :: StringIndex -> Note -> Bool
matchesString sIdx n = noteString n == Just sIdx
