module Termtab.UI.Types (
    AppState (..),
    DisplayMode (..),
    InputMode (..),
    ResourceName (..),
    AppEvent (..),
    initAppState,
    -- Pure navigation functions
    moveBeatRight,
    moveBeatLeft,
    moveMeasureForward,
    moveMeasureBack,
    moveStringUp,
    moveStringDown,
    goToStart,
    goToEnd,
    goToMeasureStart,
    goToMeasureEnd,
    goToFirstString,
    goToLastString,
    cycleDisplayMode,
    zoomIn,
    zoomOut,
    -- Helpers
    currentTrack,
    currentMeasure,
    currentMeasureBeatCount,
    atLastBeat,
    atLastMeasure,
    atFirstBeat,
    atFirstMeasure,
    isMeasureEmpty,
    beatsForTrackMeasure,
    stringCountForTrack,
    measureCount,
) where

import Brick.BChan (BChan)
import Data.Map.Strict qualified as Map
import Termtab.Audio.PlaybackThread (PlaybackEnv)
import Termtab.Audio.Types (PlaybackStatus (..))
import Termtab.Types

data DisplayMode
    = TabOnly
    | NotationOnly
    | TabAndNotation
    deriving (Show, Eq)

data InputMode
    = NormalMode
    | CommandMode String
    | GoToMode
    | VisualMode
    | MenuMode [String] -- menu path, e.g. [] = root, ["s"] = subdivide submenu
    deriving (Show, Eq)

data ResourceName
    = TrackViewport TrackIndex
    | StatusBarResource
    | CommandLineResource
    deriving (Show, Eq, Ord)

data AppEvent
    = PlaybackTick
    deriving (Show, Eq)

data AppState = AppState
    { asSong :: Song
    , asFilePath :: Maybe FilePath
    , asCurrentMeasure :: MeasureIndex
    , asCurrentBeat :: BeatIndex
    , asCurrentTrack :: TrackIndex
    , asCurrentString :: StringIndex
    , asInputMode :: InputMode
    , asDisplayModes :: Map.Map TrackIndex DisplayMode
    , asZoom :: Int
    , asMessage :: Maybe String
    , asPlaybackStatus :: PlaybackStatus
    , asPlayheadMeasure :: Maybe MeasureIndex
    , asPlayheadBeat :: Maybe BeatIndex
    , asPlaybackEnv :: Maybe PlaybackEnv
    , asBChan :: BChan AppEvent
    , asUndoStack :: [Song]
    , asRedoStack :: [Song]
    , asFretEntry :: Maybe FretNumber
    , asSelectionStart :: Maybe (MeasureIndex, BeatIndex)
    }

initAppState :: Maybe FilePath -> Song -> BChan AppEvent -> AppState
initAppState mPath song bChan =
    AppState
        { asSong = song
        , asFilePath = mPath
        , asCurrentMeasure = MeasureIndex 0
        , asCurrentBeat = BeatIndex 0
        , asCurrentTrack = TrackIndex 0
        , asCurrentString = StringIndex 0
        , asInputMode = NormalMode
        , asDisplayModes = Map.fromList defaultModes
        , asZoom = 4
        , asMessage = Nothing
        , asPlaybackStatus = Stopped
        , asPlayheadMeasure = Nothing
        , asPlayheadBeat = Nothing
        , asPlaybackEnv = Nothing
        , asBChan = bChan
        , asUndoStack = []
        , asRedoStack = []
        , asFretEntry = Nothing
        , asSelectionStart = Nothing
        }
  where
    defaultModes =
        [ (TrackIndex i, defaultModeFor (trackInstrument t))
        | (i, t) <- zip [0 ..] (songTracks song)
        ]
    defaultModeFor (Guitar _ _) = TabOnly
    defaultModeFor (Bass _ _) = TabOnly
    defaultModeFor (Standard _) = NotationOnly

-- Helpers

currentTrack :: AppState -> Maybe Track
currentTrack st =
    let TrackIndex i = asCurrentTrack st
     in case drop i (songTracks (asSong st)) of
            (t : _) -> Just t
            [] -> Nothing

currentMeasure :: AppState -> Maybe Measure
currentMeasure st =
    let MeasureIndex i = asCurrentMeasure st
     in case drop i (songMeasures (asSong st)) of
            (m : _) -> Just m
            [] -> Nothing

beatsForTrackMeasure :: Track -> MeasureIndex -> [Beat]
beatsForTrackMeasure track mi =
    maybe [] id (Map.lookup mi (trackBeats track))

stringCountForTrack :: Track -> Int
stringCountForTrack track = case trackInstrument track of
    Guitar _ n -> n
    Bass _ n -> n
    Standard _ -> 0

measureCount :: AppState -> Int
measureCount st = length (songMeasures (asSong st))

{- | A measure is empty if no track has any notes in it.
Empty beat lists or beats with no notes all count as empty.
-}
isMeasureEmpty :: MeasureIndex -> AppState -> Bool
isMeasureEmpty mi st =
    all trackEmpty (songTracks (asSong st))
  where
    trackEmpty track =
        let beats = beatsForTrackMeasure track mi
         in all beatEmpty beats
    beatEmpty beat = null (beatNotes beat)

-- Navigation

{- | Number of navigable beats in the current measure.
Accounts for actual beats plus empty slots that fill the remaining
duration from the time signature.
-}
currentMeasureBeatCount :: AppState -> Int
currentMeasureBeatCount st =
    let timeSigBeats = case currentMeasure st of
            Just m -> let TimeSignature num _ = timeSignature m in num
            Nothing -> 4
        beats = case currentTrack st of
            Just t -> beatsForTrackMeasure t (asCurrentMeasure st)
            Nothing -> []
        actualCount = length beats
        -- Duration units used by actual beats (quarter = 4 units)
        usedUnits = sum (map (durationWeightForNav . beatDuration) beats)
        totalUnits = timeSigBeats * 4 -- each quarter = 4 units
        remainingUnits = max 0 (totalUnits - usedUnits)
        emptySlots = remainingUnits `div` 4 -- each empty slot = 1 quarter
     in actualCount + emptySlots

-- | Duration weight for navigation (quarter = 4 units, matching the renderer).
durationWeightForNav :: Duration -> Int
durationWeightForNav Whole = 16
durationWeightForNav Half = 8
durationWeightForNav Quarter = 4
durationWeightForNav Eighth = 2
durationWeightForNav Sixteenth = 1
durationWeightForNav Thirty2nd = 1
durationWeightForNav (Dotted d) = durationWeightForNav d + durationWeightForNav d `div` 2
durationWeightForNav (Triplet d) = durationWeightForNav d * 2 `div` 3

-- | Are we at the last beat of the current measure?
atLastBeat :: AppState -> Bool
atLastBeat st =
    let BeatIndex b = asCurrentBeat st
     in b >= currentMeasureBeatCount st - 1

-- | Are we at the last measure?
atLastMeasure :: AppState -> Bool
atLastMeasure st =
    let MeasureIndex m = asCurrentMeasure st
     in m >= measureCount st - 1

moveBeatRight :: AppState -> AppState
moveBeatRight st
    | atLastBeat st && not (atLastMeasure st) =
        -- Wrap to next measure
        st{asCurrentMeasure = let MeasureIndex m = asCurrentMeasure st in MeasureIndex (m + 1), asCurrentBeat = BeatIndex 0}
    | atLastBeat st = st -- at end of last measure, handled by keybinding for auto-create
    | otherwise =
        let BeatIndex b = asCurrentBeat st
         in st{asCurrentBeat = BeatIndex (b + 1)}

-- | Are we at the first beat of the current measure?
atFirstBeat :: AppState -> Bool
atFirstBeat st = asCurrentBeat st == BeatIndex 0

-- | Are we at the first measure?
atFirstMeasure :: AppState -> Bool
atFirstMeasure st = asCurrentMeasure st == MeasureIndex 0

moveBeatLeft :: AppState -> AppState
moveBeatLeft st
    | atFirstBeat st && not (atFirstMeasure st) =
        -- Wrap to last beat of previous measure
        let MeasureIndex m = asCurrentMeasure st
            prevM = MeasureIndex (m - 1)
            -- Temporarily set cursor to previous measure to compute its beat count
            prevSt = st{asCurrentMeasure = prevM}
            prevBeatCount = currentMeasureBeatCount prevSt
         in st{asCurrentMeasure = prevM, asCurrentBeat = BeatIndex (prevBeatCount - 1)}
    | atFirstBeat st = st
    | otherwise =
        let BeatIndex b = asCurrentBeat st
         in st{asCurrentBeat = BeatIndex (b - 1)}

moveMeasureForward :: AppState -> AppState
moveMeasureForward st =
    let MeasureIndex m = asCurrentMeasure st
        maxM = max 0 (measureCount st - 1)
     in st
            { asCurrentMeasure = MeasureIndex (min (m + 1) maxM)
            , asCurrentBeat = BeatIndex 0
            }

moveMeasureBack :: AppState -> AppState
moveMeasureBack st =
    let MeasureIndex m = asCurrentMeasure st
     in st
            { asCurrentMeasure = MeasureIndex (max 0 (m - 1))
            , asCurrentBeat = BeatIndex 0
            }

moveStringUp :: AppState -> AppState
moveStringUp st =
    let StringIndex s = asCurrentString st
     in st{asCurrentString = StringIndex (max 0 (s - 1))}

moveStringDown :: AppState -> AppState
moveStringDown st =
    let StringIndex s = asCurrentString st
        maxS = case currentTrack st of
            Nothing -> 0
            Just t -> max 0 (stringCountForTrack t - 1)
     in st{asCurrentString = StringIndex (min (s + 1) maxS)}

goToStart :: AppState -> AppState
goToStart st =
    st
        { asCurrentMeasure = MeasureIndex 0
        , asCurrentBeat = BeatIndex 0
        }

goToEnd :: AppState -> AppState
goToEnd st =
    let lastM = max 0 (measureCount st - 1)
        measures = songMeasures (asSong st)
        lastBeat = case drop lastM measures of
            (m : _) -> let TimeSignature num _ = timeSignature m in num - 1
            [] -> 0
     in st
            { asCurrentMeasure = MeasureIndex lastM
            , asCurrentBeat = BeatIndex lastBeat
            }

goToMeasureStart :: AppState -> AppState
goToMeasureStart st = st{asCurrentBeat = BeatIndex 0}

goToMeasureEnd :: AppState -> AppState
goToMeasureEnd st =
    let lastBeat = case currentMeasure st of
            Just m -> let TimeSignature num _ = timeSignature m in num - 1
            Nothing -> 0
     in st{asCurrentBeat = BeatIndex lastBeat}

goToFirstString :: AppState -> AppState
goToFirstString st = st{asCurrentString = StringIndex 0}

goToLastString :: AppState -> AppState
goToLastString st =
    let maxS = case currentTrack st of
            Nothing -> 0
            Just t -> max 0 (stringCountForTrack t - 1)
     in st{asCurrentString = StringIndex maxS}

cycleDisplayMode :: AppState -> AppState
cycleDisplayMode st =
    let tIdx = asCurrentTrack st
        current = Map.findWithDefault TabOnly tIdx (asDisplayModes st)
        next = case current of
            TabOnly -> NotationOnly
            NotationOnly -> TabAndNotation
            TabAndNotation -> TabOnly
     in st{asDisplayModes = Map.insert tIdx next (asDisplayModes st)}

zoomIn :: AppState -> AppState
zoomIn st = st{asZoom = min 12 (asZoom st + 1)}

zoomOut :: AppState -> AppState
zoomOut st = st{asZoom = max 2 (asZoom st - 1)}
