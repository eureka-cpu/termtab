module Termtab.Audio.PlaybackThread (
    PlaybackCommand (..),
    PlaybackEnv (..),
    initPlaybackEnv,
    destroyPlaybackEnv,
    startPlayback,
    pausePlayback,
    stopPlayback,
    resumePlayback,
) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, catch)
import Control.Monad (forM_)
import Data.Map.Strict qualified as Map

import Termtab.Audio (AudioEngine, engineNoteOff, engineNoteOn, startAudioEngine, stopAudioEngine)
import Termtab.Audio.Playback (durationToMicroseconds)
import Termtab.Audio.Types (AudioConfig, BackendType (..), PlaybackStatus (..))
import Termtab.Types

data PlaybackCommand
    = CmdStartPlayback MeasureIndex
    | CmdPausePlayback
    | CmdStopPlayback
    deriving (Show, Eq)

data PlaybackEnv = PlaybackEnv
    { peAudioEngine :: AudioEngine
    , pePositionVar :: TVar (MeasureIndex, BeatIndex)
    , peCommandVar :: TVar (Maybe PlaybackCommand)
    , peStatusVar :: TVar PlaybackStatus
    , peNotify :: IO ()
    -- ^ Called after each position update to wake the UI
    , peThreadId :: TVar (Maybe ThreadId)
    }

initPlaybackEnv :: AudioConfig -> IO () -> IO PlaybackEnv
initPlaybackEnv cfg notify = do
    engine <- startAudioEngine BackendFluidSynth cfg
    posVar <- newTVarIO (MeasureIndex 0, BeatIndex 0)
    cmdVar <- newTVarIO Nothing
    statusVar <- newTVarIO Stopped
    tidVar <- newTVarIO Nothing
    return
        PlaybackEnv
            { peAudioEngine = engine
            , pePositionVar = posVar
            , peCommandVar = cmdVar
            , peStatusVar = statusVar
            , peNotify = notify
            , peThreadId = tidVar
            }

destroyPlaybackEnv :: PlaybackEnv -> IO ()
destroyPlaybackEnv env = do
    mTid <- atomically $ readTVar (peThreadId env)
    mapM_ killThread mTid
    stopAudioEngine (peAudioEngine env)

startPlayback :: PlaybackEnv -> Song -> MeasureIndex -> IO ()
startPlayback env song startMeasure = do
    -- Kill any existing thread
    mOldTid <- atomically $ readTVar (peThreadId env)
    mapM_ killThread mOldTid
    -- Spawn tick thread
    tid <- forkIO $ tickThread env song startMeasure
    atomically $ do
        writeTVar (peThreadId env) (Just tid)
        writeTVar (peStatusVar env) Playing

pausePlayback :: PlaybackEnv -> IO ()
pausePlayback env =
    atomically $
        writeTVar (peCommandVar env) (Just CmdPausePlayback)

stopPlayback :: PlaybackEnv -> IO ()
stopPlayback env =
    atomically $
        writeTVar (peCommandVar env) (Just CmdStopPlayback)

resumePlayback :: PlaybackEnv -> MeasureIndex -> IO ()
resumePlayback env mi =
    atomically $
        writeTVar (peCommandVar env) (Just (CmdStartPlayback mi))

-- Internal

tickThread :: PlaybackEnv -> Song -> MeasureIndex -> IO ()
tickThread env song (MeasureIndex startMi) =
    go (songTempo song) startMi
        `catch` \(_ :: SomeException) ->
            atomically $ writeTVar (peStatusVar env) Stopped
  where
    measures = songMeasures song
    tracks = songTracks song
    totalMeasures = length measures

    -- Compute total playable beats: actual beats + empty slots from remaining duration
    playableBeatCount :: Measure -> MeasureIndex -> Int
    playableBeatCount measure mi =
        let TimeSignature num _ = timeSignature measure
            getBeats t = maybe [] id (Map.lookup mi (trackBeats t))
            -- Use the longest beat list among all tracks
            maxActual = maximum (num : map (length . getBeats) tracks)
            longestBeats = case filter (\t -> length (getBeats t) == maxActual) tracks of
                (t : _) -> getBeats t
                [] -> []
            usedUnits = sum (map (beatDurWeight . beatDuration) longestBeats)
            totalUnits = num * 4
            remainingUnits = max 0 (totalUnits - usedUnits)
            emptySlots = remainingUnits `div` 4
         in maxActual + emptySlots

    beatDurWeight :: Duration -> Int
    beatDurWeight Whole = 16
    beatDurWeight Half = 8
    beatDurWeight Quarter = 4
    beatDurWeight Eighth = 2
    beatDurWeight Sixteenth = 1
    beatDurWeight Thirty2nd = 1
    beatDurWeight (Dotted d) = beatDurWeight d + beatDurWeight d `div` 2
    beatDurWeight (Triplet d) = beatDurWeight d * 2 `div` 3

    go :: Tempo -> Int -> IO ()
    go _tempo mi
        | mi >= totalMeasures = do
            -- Song ended
            atomically $ do
                writeTVar (peStatusVar env) Stopped
                writeTVar (pePositionVar env) (MeasureIndex 0, BeatIndex 0)
            peNotify env
    go tempo mi = do
        let measure = measures !! mi
            currentTempo = maybe tempo id (tempoChange measure)
            totalBeats = playableBeatCount measure (MeasureIndex mi)
        completed <- playMeasureBeats currentTempo (MeasureIndex mi) 0 totalBeats
        if completed
            then go currentTempo (mi + 1)
            else return ()

    playMeasureBeats :: Tempo -> MeasureIndex -> Int -> Int -> IO Bool
    playMeasureBeats _tempo _mi beatIdx numBeats
        | beatIdx >= numBeats = return True
    playMeasureBeats tempo mi beatIdx numBeats = do
        -- Check for commands
        mCmd <- atomically $ swapTVar (peCommandVar env) Nothing
        case mCmd of
            Just CmdStopPlayback -> do
                allNotesOff env tracks
                atomically $ writeTVar (peStatusVar env) Stopped
                peNotify env
                return False
            Just CmdPausePlayback -> do
                allNotesOff env tracks
                atomically $ do
                    writeTVar (peStatusVar env) Paused
                    writeTVar (pePositionVar env) (mi, BeatIndex beatIdx)
                peNotify env
                resumed <- waitForResume env
                if resumed
                    then playMeasureBeats tempo mi beatIdx numBeats
                    else return False
            _ -> do
                let bi = BeatIndex beatIdx
                -- Update position
                atomically $ writeTVar (pePositionVar env) (mi, bi)
                peNotify env

                -- Collect beat info and fire NoteOn
                let beatDur = findBeatDuration tracks mi beatIdx
                forM_ tracks $ \track ->
                    case getBeatAt track mi beatIdx of
                        Just beat | not (beatIsRest beat) ->
                            forM_ (beatNotes beat) $ \note ->
                                engineNoteOn
                                    (peAudioEngine env)
                                    (trackChannel track)
                                    (notePitch note)
                                    (noteVelocity note)
                        _ -> return ()

                -- Wait for beat duration
                threadDelay (durationToMicroseconds beatDur tempo)

                -- Fire NoteOff
                forM_ tracks $ \track ->
                    case getBeatAt track mi beatIdx of
                        Just beat | not (beatIsRest beat) ->
                            forM_ (beatNotes beat) $ \note ->
                                engineNoteOff
                                    (peAudioEngine env)
                                    (trackChannel track)
                                    (notePitch note)
                        _ -> return ()

                playMeasureBeats tempo mi (beatIdx + 1) numBeats

waitForResume :: PlaybackEnv -> IO Bool
waitForResume env = atomically $ do
    mCmd <- readTVar (peCommandVar env)
    case mCmd of
        Just (CmdStartPlayback _) -> do
            writeTVar (peCommandVar env) Nothing
            writeTVar (peStatusVar env) Playing
            return True
        Just CmdStopPlayback -> do
            writeTVar (peCommandVar env) Nothing
            writeTVar (peStatusVar env) Stopped
            return False
        _ -> retry

getBeatAt :: Track -> MeasureIndex -> Int -> Maybe Beat
getBeatAt track mi beatIdx =
    case Map.lookup mi (trackBeats track) of
        Just beats | beatIdx < length beats -> Just (beats !! beatIdx)
        _ -> Nothing

findBeatDuration :: [Track] -> MeasureIndex -> Int -> Duration
findBeatDuration tracks mi beatIdx =
    let durations =
            [ beatDuration b
            | track <- tracks
            , Just b <- [getBeatAt track mi beatIdx]
            ]
     in case durations of
            [] -> Quarter
            (d : _) -> d

allNotesOff :: PlaybackEnv -> [Track] -> IO ()
allNotesOff env tracks = do
    let channels = map trackChannel tracks
    forM_ channels $ \ch ->
        forM_ [0 .. 127 :: Int] $ \p ->
            engineNoteOff (peAudioEngine env) ch (Pitch p)
