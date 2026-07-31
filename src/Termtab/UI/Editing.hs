module Termtab.UI.Editing (
    -- Editing operations
    enterFretDigit,
    deleteNoteAtCursor,
    insertRest,
    cycleDuration,
    subdivideBeat,
    combineBeats,
    addMeasure,
    deleteMeasure,
    -- Undo/redo
    withUndo,
    undo,
    redo,
    clearFretEntry,
    -- Helpers
    fretToPitch,
    modifyTrack,
    modifyBeats,
) where

import Data.Map.Strict qualified as Map
import Termtab.Defaults (defaultMeasureBeats)
import Termtab.Types
import Termtab.UI.Types

-- Nested update helpers

modifyTrack :: TrackIndex -> (Track -> Track) -> Song -> Song
modifyTrack (TrackIndex tIdx) f song =
    song{songTracks = updateAt tIdx f (songTracks song)}

modifyBeats :: MeasureIndex -> ([Beat] -> [Beat]) -> Track -> Track
modifyBeats mi f track =
    track{trackBeats = Map.alter (Just . f . maybe [] id) mi (trackBeats track)}

modifyBeatAt :: BeatIndex -> (Beat -> Beat) -> [Beat] -> [Beat]
modifyBeatAt (BeatIndex bIdx) f beats =
    let padded = ensureLength (bIdx + 1) beats
     in updateAt bIdx f padded

setNoteOnString :: StringIndex -> Maybe Note -> Beat -> Beat
setNoteOnString sIdx mNote beat =
    let others = filter (\n -> noteString n /= Just sIdx) (beatNotes beat)
        newNotes = case mNote of
            Just n -> n : others
            Nothing -> others
     in beat{beatNotes = newNotes, beatIsRest = False}

-- Pitch calculation

fretToPitch :: Instrument -> StringIndex -> FretNumber -> Pitch
fretToPitch instr (StringIndex s) (FretNumber f) =
    case tuningPitches instr of
        Just pitches
            | s >= 0
            , s < length pitches ->
                let Pitch base = pitches !! s in Pitch (base + f)
        _ -> Pitch f
  where
    tuningPitches (Guitar t _) = Just t
    tuningPitches (Bass t _) = Just t
    tuningPitches (Standard _) = Nothing

-- Editing operations

enterFretDigit :: Int -> AppState -> AppState
enterFretDigit digit st =
    case currentTrack st of
        Nothing -> st
        Just track ->
            let sIdx = asCurrentString st
                mIdx = asCurrentMeasure st
                bIdx = asCurrentBeat st
                -- Multi-digit fret accumulation
                fret = case asFretEntry st of
                    Just (FretNumber prev) ->
                        let combined = prev * 10 + digit
                         in if combined <= 24 then combined else digit
                    Nothing -> digit
                fretNum = FretNumber fret
                pitch = fretToPitch (trackInstrument track) sIdx fretNum
                note =
                    Note
                        { notePitch = pitch
                        , noteVelocity = Velocity 100
                        , noteEffects = []
                        , noteString = Just sIdx
                        , noteFret = Just fretNum
                        }
                editSong =
                    modifyTrack (asCurrentTrack st) $
                        modifyBeats mIdx $
                            modifyBeatAt bIdx $
                                setNoteOnString sIdx (Just note)
             in withUndo editSong st{asFretEntry = Just fretNum}

deleteNoteAtCursor :: AppState -> AppState
deleteNoteAtCursor st =
    case currentTrack st of
        Nothing -> st
        Just _track ->
            let sIdx = asCurrentString st
                editSong =
                    modifyTrack (asCurrentTrack st) $
                        modifyBeats (asCurrentMeasure st) $
                            modifyBeatAt (asCurrentBeat st) $
                                setNoteOnString sIdx Nothing
             in withUndo editSong st

insertRest :: AppState -> AppState
insertRest st =
    let editSong =
            modifyTrack (asCurrentTrack st) $
                modifyBeats (asCurrentMeasure st) $
                    modifyBeatAt (asCurrentBeat st) $
                        \beat -> beat{beatIsRest = True, beatNotes = []}
     in withUndo editSong st

cycleDuration :: AppState -> AppState
cycleDuration st =
    let editSong =
            modifyTrack (asCurrentTrack st) $
                modifyBeats (asCurrentMeasure st) $
                    modifyBeatAt (asCurrentBeat st) $
                        \beat -> beat{beatDuration = nextDuration (beatDuration beat)}
     in withUndo editSong st

nextDuration :: Duration -> Duration
nextDuration Whole = Half
nextDuration Half = Quarter
nextDuration Quarter = Eighth
nextDuration Eighth = Sixteenth
nextDuration Sixteenth = Thirty2nd
nextDuration Thirty2nd = Whole
nextDuration (Dotted d) = nextDuration d
nextDuration (Triplet d) = nextDuration d

-- Undo/Redo

withUndo :: (Song -> Song) -> AppState -> AppState
withUndo f st =
    st
        { asSong = f (asSong st)
        , asUndoStack = take 100 (asSong st : asUndoStack st)
        , asRedoStack = []
        }

undo :: AppState -> AppState
undo st = case asUndoStack st of
    [] -> st{asMessage = Just "Nothing to undo"}
    (prev : rest) ->
        st
            { asSong = prev
            , asUndoStack = rest
            , asRedoStack = take 100 (asSong st : asRedoStack st)
            , asMessage = Just "Undo"
            }

redo :: AppState -> AppState
redo st = case asRedoStack st of
    [] -> st{asMessage = Just "Nothing to redo"}
    (next : rest) ->
        st
            { asSong = next
            , asRedoStack = rest
            , asUndoStack = take 100 (asSong st : asUndoStack st)
            , asMessage = Just "Redo"
            }

clearFretEntry :: AppState -> AppState
clearFretEntry st = st{asFretEntry = Nothing}

-- Internal helpers

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt _ _ [] = []
updateAt 0 f (x : xs) = f x : xs
updateAt n f (x : xs) = x : updateAt (n - 1) f xs

defaultBeat :: Beat
defaultBeat = Beat{beatDuration = Quarter, beatNotes = [], beatIsRest = True}

ensureLength :: Int -> [Beat] -> [Beat]
ensureLength n beats
    | length beats >= n = beats
    | otherwise = beats ++ replicate (n - length beats) defaultBeat

-- Measure operations

addMeasure :: AppState -> AppState
addMeasure st =
    let song = asSong st
        MeasureIndex curM = asCurrentMeasure st
        -- Inherit time/key sig from current measure
        curMeasure = case currentMeasure st of
            Just m -> m
            Nothing -> Measure (MeasureIndex 0) (TimeSignature 4 4) (KeySignature 0 Major) Nothing
        newMIdx = MeasureIndex (curM + 1)
        newMeasure =
            curMeasure
                { measureIndex = newMIdx
                , tempoChange = Nothing
                }
        -- Insert measure into songMeasures after current position
        (before, after) = splitAt (curM + 1) (songMeasures song)
        -- Re-index measures after insertion
        reindexed = zipWith (\i m -> m{measureIndex = MeasureIndex i}) [0 ..] (before ++ [newMeasure] ++ after)
        -- Add default beats for the new measure in all tracks
        newBeats = defaultMeasureBeats (timeSignature curMeasure)
        updatedTracks =
            map
                ( \track ->
                    -- Shift existing beat map entries after the insertion point
                    let shifted = Map.mapKeys (\(MeasureIndex k) -> if k > curM then MeasureIndex (k + 1) else MeasureIndex k) (trackBeats track)
                     in track{trackBeats = Map.insert newMIdx newBeats shifted}
                )
                (songTracks song)
        newSong = song{songMeasures = reindexed, songTracks = updatedTracks}
     in withUndo (const newSong) st

deleteMeasure :: AppState -> AppState
deleteMeasure st
    | measureCount st <= 1 = st{asMessage = Just "Cannot delete last measure"}
    | otherwise =
        let song = asSong st
            MeasureIndex curM = asCurrentMeasure st
            -- Remove measure from songMeasures
            measures' = [m | (i, m) <- zip [0 :: Int ..] (songMeasures song), i /= curM]
            reindexed = zipWith (\i m -> m{measureIndex = MeasureIndex i}) [0 ..] measures'
            -- Remove beats and shift indices
            updatedTracks =
                map
                    ( \track ->
                        let beats' = Map.delete (MeasureIndex curM) (trackBeats track)
                            shifted = Map.mapKeys (\(MeasureIndex k) -> if k > curM then MeasureIndex (k - 1) else MeasureIndex k) beats'
                         in track{trackBeats = shifted}
                    )
                    (songTracks song)
            newSong = song{songMeasures = reindexed, songTracks = updatedTracks}
            -- Adjust cursor if past end
            newM = min curM (length reindexed - 1)
         in (withUndo (const newSong) st){asCurrentMeasure = MeasureIndex newM, asCurrentBeat = BeatIndex 0}

-- Subdivide / Combine

subdivideBeat :: Int -> AppState -> AppState
subdivideBeat n st
    | n < 2 = st
    | otherwise =
        let editSong =
                modifyTrack (asCurrentTrack st) $
                    modifyBeats (asCurrentMeasure st) $
                        \beats ->
                            let BeatIndex bIdx = asCurrentBeat st
                                padded = ensureLength (bIdx + 1) beats
                                (before, rest) = splitAt bIdx padded
                             in case rest of
                                    (beat : after) ->
                                        let subDur = subdivideDuration n (beatDuration beat)
                                            subBeats = replicate n beat{beatDuration = subDur}
                                         in before ++ subBeats ++ after
                                    [] -> padded
         in withUndo editSong st

subdivideDuration :: Int -> Duration -> Duration
subdivideDuration 2 Whole = Half
subdivideDuration 2 Half = Quarter
subdivideDuration 2 Quarter = Eighth
subdivideDuration 2 Eighth = Sixteenth
subdivideDuration 2 Sixteenth = Thirty2nd
subdivideDuration 2 _ = Thirty2nd -- can't subdivide further
subdivideDuration 3 d = Triplet (subdivideDuration 2 d) -- triplets use the half-value wrapped
subdivideDuration 4 d = subdivideDuration 2 (subdivideDuration 2 d)
subdivideDuration _ d = d

combineBeats :: AppState -> AppState
combineBeats st = case asSelectionStart st of
    Nothing -> st{asMessage = Just "Select beats first (v)"}
    Just (selMi, selBi) ->
        let mi = asCurrentMeasure st
         in if selMi /= mi
                then st{asMessage = Just "Selection must be within one measure"}
                else
                    let BeatIndex startB = min selBi (asCurrentBeat st)
                        BeatIndex endB = max selBi (asCurrentBeat st)
                        editSong =
                            modifyTrack (asCurrentTrack st) $
                                modifyBeats mi $
                                    \beats ->
                                        let padded = ensureLength (endB + 1) beats
                                            (before, rest) = splitAt startB padded
                                            (selected, after) = splitAt (endB - startB + 1) rest
                                            combinedNotes = concatMap beatNotes selected
                                            combinedDur = foldl1 addDurations (map beatDuration selected)
                                            combined =
                                                Beat
                                                    { beatDuration = combinedDur
                                                    , beatNotes = combinedNotes
                                                    , beatIsRest = null combinedNotes
                                                    }
                                         in before ++ [combined] ++ after
                     in (withUndo editSong st)
                            { asCurrentBeat = BeatIndex startB
                            , asInputMode = NormalMode
                            , asSelectionStart = Nothing
                            , asMessage = Just "Combined"
                            }

-- | Approximate duration addition — returns the closest standard duration.
addDurations :: Duration -> Duration -> Duration
addDurations a b =
    let total = durationValue a + durationValue b
     in fromDurationValue total

durationValue :: Duration -> Int
durationValue Whole = 16
durationValue Half = 8
durationValue Quarter = 4
durationValue Eighth = 2
durationValue Sixteenth = 1
durationValue Thirty2nd = 1 -- approximate
durationValue (Dotted d) = durationValue d + durationValue d `div` 2
durationValue (Triplet d) = durationValue d * 2 `div` 3

fromDurationValue :: Int -> Duration
fromDurationValue v
    | v >= 16 = Whole
    | v >= 8 = Half
    | v >= 4 = Quarter
    | v >= 2 = Eighth
    | otherwise = Sixteenth
