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

-- Navigation

moveBeatRight :: AppState -> AppState
moveBeatRight st =
    let BeatIndex b = asCurrentBeat st
        maxBeat = case currentTrack st of
            Nothing -> 0
            Just t ->
                let beats = beatsForTrackMeasure t (asCurrentMeasure st)
                 in max 0 (length beats - 1)
     in st{asCurrentBeat = BeatIndex (min (b + 1) maxBeat)}

moveBeatLeft :: AppState -> AppState
moveBeatLeft st =
    let BeatIndex b = asCurrentBeat st
     in st{asCurrentBeat = BeatIndex (max 0 (b - 1))}

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
        lastBeat = case currentTrack st of
            Nothing -> 0
            Just t ->
                let beats = beatsForTrackMeasure t (MeasureIndex lastM)
                 in max 0 (length beats - 1)
     in st
            { asCurrentMeasure = MeasureIndex lastM
            , asCurrentBeat = BeatIndex lastBeat
            }

goToMeasureStart :: AppState -> AppState
goToMeasureStart st = st{asCurrentBeat = BeatIndex 0}

goToMeasureEnd :: AppState -> AppState
goToMeasureEnd st =
    let lastBeat = case currentTrack st of
            Nothing -> 0
            Just t ->
                let beats = beatsForTrackMeasure t (asCurrentMeasure st)
                 in max 0 (length beats - 1)
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
