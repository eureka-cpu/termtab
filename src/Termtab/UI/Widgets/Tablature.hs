module Termtab.UI.Widgets.Tablature (renderTablature, cursorAttr, playheadAttr, barLineAttr, stringLabelAttr) where

import Brick hiding (zoom)
import Data.Map.Strict qualified as Map

import Termtab.Types
import Termtab.UI.Types

cursorAttr :: AttrName
cursorAttr = attrName "cursor"

playheadAttr :: AttrName
playheadAttr = attrName "playhead"

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
        -- Determine visible measures starting from current
        MeasureIndex startM = asCurrentMeasure st
        visibleMeasures = fitMeasures measures zoom contentWidth startM
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

-- | How many beats does this measure have based on its time signature numerator
measureBeatCount :: [Measure] -> MeasureIndex -> Int
measureBeatCount measures (MeasureIndex mi) =
    case drop mi measures of
        (m : _) -> let TimeSignature num _ = timeSignature m in num
        [] -> 4

-- | Determine which measures fit in the available width
fitMeasures :: [Measure] -> Int -> Int -> Int -> [(MeasureIndex, Int)]
fitMeasures measures zoom availW startM = go startM availW
  where
    totalM = length measures
    go mi remaining
        | mi >= totalM = []
        | otherwise =
            let nBeats = measureBeatCount measures (MeasureIndex mi)
                mWidth = nBeats * zoom + 1 -- +1 for bar line
             in if mWidth > remaining && mi > startM
                    then []
                    else (MeasureIndex mi, nBeats) : go (mi + 1) (remaining - mWidth)

renderStringLine ::
    AppState ->
    TrackIndex ->
    Track ->
    Bool ->
    [String] ->
    Int ->
    Int ->
    [(MeasureIndex, Int)] ->
    StringIndex ->
    Widget ResourceName
renderStringLine st tIdx track isFocused labels labelWidth zoom visibleMeasures sIdx =
    hBox $
        [ withAttr stringLabelAttr $ str (padLabel labelWidth (getLabel labels sIdx) ++ "|")
        ]
            ++ concatMap (renderMeasureOnString st tIdx track isFocused zoom sIdx) visibleMeasures

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
    TrackIndex ->
    Track ->
    Bool ->
    Int ->
    StringIndex ->
    (MeasureIndex, Int) ->
    [Widget ResourceName]
renderMeasureOnString st tIdx track isFocused zoom sIdx (mi, nBeats) =
    let beats = beatsForTrackMeasure track mi
        beatWidgets =
            [ renderBeatCell st tIdx isFocused zoom sIdx mi (BeatIndex bi) beats
            | bi <- [0 .. nBeats - 1]
            ]
        barLine = withAttr barLineAttr (str "|")
     in beatWidgets ++ [barLine]

renderBeatCell ::
    AppState ->
    TrackIndex ->
    Bool ->
    Int ->
    StringIndex ->
    MeasureIndex ->
    BeatIndex ->
    [Beat] ->
    Widget ResourceName
renderBeatCell st _tIdx isFocused zoom sIdx mi bi beats =
    let BeatIndex bIdx = bi
        fretText = case drop bIdx beats of
            (beat : _) -> fretOnString sIdx beat
            [] -> Nothing
        cell = case fretText of
            Just f -> padCell zoom (show f)
            Nothing -> replicate zoom '-'
        isCursor =
            isFocused
                && mi == asCurrentMeasure st
                && bi == asCurrentBeat st
                && sIdx == asCurrentString st
        isPlayhead =
            asPlayheadMeasure st == Just mi
                && asPlayheadBeat st == Just bi
        attr
            | isCursor = cursorAttr
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
