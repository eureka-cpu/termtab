module Termtab.UI.Editing (
    -- Editing operations
    enterFretDigit,
    deleteNoteAtCursor,
    insertRest,
    cycleDuration,
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
