module Termtab.UI.Keybindings (handleEvent) where

import Brick
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (digitToInt, isDigit)
import Graphics.Vty qualified as V
import System.Environment (lookupEnv)
import System.FilePath (replaceExtension)

import Termtab.Audio (engineNoteOff, engineNoteOn)
import Termtab.Audio.PlaybackThread
import Termtab.Audio.Types (AudioConfig (..), PlaybackStatus (..))
import Termtab.Export.MIDI (exportMidi)
import Termtab.Types (Beat (..), BeatIndex (..), Measure (..), MeasureIndex (..), Note (..), Song (..), TimeSignature (..), Track (..))
import Termtab.UI.Editing
import Termtab.UI.Types

handleEvent :: BrickEvent ResourceName AppEvent -> EventM ResourceName AppState ()
handleEvent (VtyEvent (V.EvKey key mods)) = do
    mode <- gets asInputMode
    case mode of
        NormalMode -> handleNormal key mods
        GoToMode -> handleGoTo key
        VisualMode -> handleVisual key
        MenuMode path -> handleMenu path key
        CommandMode buf -> handleCommand key buf
handleEvent (AppEvent PlaybackTick) = handlePlaybackTick
handleEvent _ = return ()

handlePlaybackTick :: EventM ResourceName AppState ()
handlePlaybackTick = do
    mEnv <- gets asPlaybackEnv
    case mEnv of
        Nothing -> return ()
        Just env -> do
            (mi, bi) <- liftIO $ readTVarIO (pePositionVar env)
            status <- liftIO $ readTVarIO (peStatusVar env)
            modify $ \st ->
                st
                    { asPlayheadMeasure = case status of
                        Stopped -> Nothing
                        _ -> Just mi
                    , asPlayheadBeat = case status of
                        Stopped -> Nothing
                        _ -> Just bi
                    , asPlaybackStatus = status
                    }

handleNormal :: V.Key -> [V.Modifier] -> EventM ResourceName AppState ()
handleNormal key mods = case key of
    -- Quit / mode switching
    V.KEsc -> handleEsc
    V.KChar ':' -> modify $ clearFretEntry . \st -> st{asInputMode = CommandMode "", asMessage = Nothing}
    -- Navigation (clears fret entry)
    V.KChar 'h' -> handleMoveLeft
    V.KChar 'l' | V.MCtrl `notElem` mods -> handleMoveRight
    V.KChar 'k' -> modify $ clearFretEntry . clearMessage . moveStringDown
    V.KChar 'j' -> modify $ clearFretEntry . clearMessage . moveStringUp
    V.KChar 'W' -> handleMoveForward
    V.KChar 'B' -> handleMoveBack
    V.KChar 'g' -> modify $ clearFretEntry . \st -> st{asInputMode = GoToMode, asMessage = Just "g-"}
    -- Visual mode
    V.KChar 'v' ->
        modify $
            clearFretEntry . \st ->
                st{asInputMode = VisualMode, asSelectionStart = Just (asCurrentMeasure st, asCurrentBeat st), asMessage = Just "-- VISUAL --"}
    -- Space menu
    V.KChar ' ' -> modify $ clearFretEntry . \st -> st{asInputMode = MenuMode [], asMessage = Just "s:subdivide  c:combine  n:note  m:measure"}
    -- Display
    V.KChar 't' -> modify $ clearFretEntry . clearMessage . cycleDisplayMode
    V.KChar '+' -> modify $ clearFretEntry . clearMessage . zoomIn
    V.KChar '-' -> modify $ clearFretEntry . clearMessage . zoomOut
    -- Editing: fret entry
    V.KChar c | isDigit c -> do
        modify $ clearMessage . enterFretDigit (digitToInt c)
        previewNote
    -- Editing: delete
    V.KChar 'd' -> modify $ clearFretEntry . clearMessage . deleteNoteAtCursor
    V.KBS -> modify $ clearFretEntry . clearMessage . deleteNoteAtCursor
    -- Editing: rest
    V.KChar 'r' -> modify $ clearFretEntry . clearMessage . insertRest
    -- Editing: duration cycle
    V.KBackTab -> modify $ clearFretEntry . clearMessage . cycleDuration
    -- Undo/Redo
    V.KChar 'u' -> modify $ clearFretEntry . undo
    V.KChar 'U' -> modify $ clearFretEntry . redo
    -- Save (Ctrl+s)
    V.KChar 's' | V.MCtrl `elem` mods -> doSave
    _ -> return ()

handleEsc :: EventM ResourceName AppState ()
handleEsc = do
    status <- gets asPlaybackStatus
    mEnv <- gets asPlaybackEnv
    case (status, mEnv) of
        (Playing, Just env) -> do
            liftIO $ pausePlayback env
            modify $ \st -> st{asPlaybackStatus = Paused, asMessage = Just "Paused"}
        (Paused, Just env) -> do
            liftIO $ stopPlayback env
            modify $ \st ->
                st
                    { asPlaybackStatus = Stopped
                    , asPlayheadMeasure = Nothing
                    , asPlayheadBeat = Nothing
                    , asMessage = Just "Stopped"
                    }
        _ -> return ()

handleMoveRight :: EventM ResourceName AppState ()
handleMoveRight = do
    st <- get
    if atLastBeat st && atLastMeasure st
        then modify $ clearFretEntry . clearMessage . moveMeasureForward . addMeasure
        else modify $ clearFretEntry . clearMessage . moveBeatRight

handleMoveLeft :: EventM ResourceName AppState ()
handleMoveLeft = do
    st <- get
    let curMi = asCurrentMeasure st
    if atFirstBeat st && not (atFirstMeasure st) && isTrailingEmpty curMi st && measureCount st > 1
        then -- Leaving a trailing empty measure: delete it and go to last beat of previous measure
            modify $ clearFretEntry . clearMessage . deleteAndGoToPrevLastBeat
        else modify $ clearFretEntry . clearMessage . moveBeatLeft

handleMoveBack :: EventM ResourceName AppState ()
handleMoveBack = do
    st <- get
    let curMi = asCurrentMeasure st
    if not (atFirstMeasure st) && isTrailingEmpty curMi st && measureCount st > 1
        then -- Delete trailing empty measure and go to last beat of previous measure
            modify $ clearFretEntry . clearMessage . deleteAndGoToPrevLastBeat
        else modify $ clearFretEntry . clearMessage . moveMeasureBack

-- | Delete the current empty measure and move cursor to the last beat of the previous measure.
deleteAndGoToPrevLastBeat :: AppState -> AppState
deleteAndGoToPrevLastBeat st =
    let MeasureIndex curM = asCurrentMeasure st
        prevM = max 0 (curM - 1)
        -- Compute last beat of previous measure BEFORE deletion
        prevSt = st{asCurrentMeasure = MeasureIndex prevM}
        prevBeatCount = currentMeasureBeatCount prevSt
        deleted = deleteMeasure st
     in deleted{asCurrentMeasure = MeasureIndex prevM, asCurrentBeat = BeatIndex (prevBeatCount - 1)}

handleMoveForward :: EventM ResourceName AppState ()
handleMoveForward = do
    st <- get
    if atLastMeasure st
        then modify $ clearFretEntry . clearMessage . moveMeasureForward . addMeasure
        else modify $ clearFretEntry . clearMessage . moveMeasureForward

handleVisual :: V.Key -> EventM ResourceName AppState ()
handleVisual key = case key of
    V.KEsc -> modify $ clearMessage . backToNormal . \st -> st{asSelectionStart = Nothing}
    V.KChar 'h' -> modify $ clearMessage . moveBeatLeft
    V.KChar 'l' -> modify $ clearMessage . moveBeatRight
    V.KChar ' ' -> modify $ \st -> st{asInputMode = MenuMode [], asMessage = Just "s:subdivide  c:combine  n:note  m:measure"}
    _ -> return ()

handleMenu :: [String] -> V.Key -> EventM ResourceName AppState ()
handleMenu [] key = case key of
    -- Root menu
    V.KChar 's' -> modify $ \st -> st{asInputMode = MenuMode ["s"], asMessage = Just "2:halves  3:triplets  4:quadruplets"}
    V.KChar 'c' -> modify $ clearMessage . combineBeats . backToNormal
    V.KChar 'n' -> modify $ \st -> st{asInputMode = MenuMode ["n"], asMessage = Just "b:bend  s:slide  h:hammer-on  p:pull-off  v:vibrato  m:palm mute (stubs)"}
    V.KChar 'm' -> modify $ \st -> st{asInputMode = MenuMode ["m"], asMessage = Just "a:add measure  d:delete measure"}
    V.KEsc -> modify $ clearMessage . backToNormal
    _ -> modify $ backToNormal . setMessage "Unknown menu option"
handleMenu ["s"] key = case key of
    -- Subdivide submenu
    V.KChar '2' -> modify $ clearMessage . backToNormal . subdivideBeat 2
    V.KChar '3' -> modify $ clearMessage . backToNormal . subdivideBeat 3
    V.KChar '4' -> modify $ clearMessage . backToNormal . subdivideBeat 4
    V.KEsc -> modify $ \st -> st{asInputMode = MenuMode [], asMessage = Just "s:subdivide  c:combine  n:note  m:measure"}
    _ -> modify $ backToNormal . setMessage "Unknown subdivision"
handleMenu ["n"] key = case key of
    -- Note effects submenu (stubs)
    V.KEsc -> modify $ \st -> st{asInputMode = MenuMode [], asMessage = Just "s:subdivide  c:combine  n:note  m:measure"}
    _ -> modify $ backToNormal . setMessage "Not yet implemented"
handleMenu ["m"] key = case key of
    -- Measure submenu
    V.KChar 'a' -> modify $ clearMessage . backToNormal . addMeasure
    V.KChar 'd' -> modify $ clearMessage . backToNormal . deleteMeasure
    V.KEsc -> modify $ \st -> st{asInputMode = MenuMode [], asMessage = Just "s:subdivide  c:combine  n:note  m:measure"}
    _ -> modify $ backToNormal . setMessage "Unknown measure operation"
handleMenu _ key = case key of
    V.KEsc -> modify $ clearMessage . backToNormal
    _ -> modify $ backToNormal . setMessage "Unknown menu option"

handleGoTo :: V.Key -> EventM ResourceName AppState ()
handleGoTo key = case key of
    V.KChar 'g' -> modify $ clearMessage . goToStart . backToNormal
    V.KChar 'e' -> modify $ clearMessage . goToEnd . backToNormal
    V.KChar 'h' -> modify $ clearMessage . goToMeasureStart . backToNormal
    V.KChar 'l' -> modify $ clearMessage . goToMeasureEnd . backToNormal
    V.KChar 'k' -> modify $ clearMessage . goToFirstString . backToNormal
    V.KChar 'j' -> modify $ clearMessage . goToLastString . backToNormal
    V.KEsc -> modify $ clearMessage . backToNormal
    _ -> modify $ backToNormal . setMessage "Unknown go-to target"

handleCommand :: V.Key -> String -> EventM ResourceName AppState ()
handleCommand key buf = case key of
    V.KEnter -> dispatchCommand buf
    V.KEsc -> modify $ clearMessage . backToNormal
    V.KBS
        | null buf -> modify $ clearMessage . backToNormal
        | otherwise -> modify $ \st -> st{asInputMode = CommandMode (init buf)}
    V.KChar c -> modify $ \st -> st{asInputMode = CommandMode (buf ++ [c])}
    _ -> return ()

dispatchCommand :: String -> EventM ResourceName AppState ()
dispatchCommand cmd = case cmd of
    "q" -> halt
    "quit" -> halt
    "exit" -> halt
    "p" -> startPlay
    "play" -> startPlay
    "w" -> doSave
    "write" -> doSave
    "save" -> doSave
    _ -> modify $ backToNormal . setMessage ("Unknown command: " ++ cmd)

doSave :: EventM ResourceName AppState ()
doSave = do
    st <- get
    let song = asSong st
        savePath = case asFilePath st of
            Just fp
                | any (`isSuffixOf'` fp) [".mid", ".midi"] -> fp
                | otherwise -> replaceExtension fp ".mid"
            Nothing -> "untitled.mid"
    result <- liftIO $ saveToFile savePath song
    case result of
        Right () -> modify $ backToNormal . setMessage ("Saved: " ++ savePath)
        Left err -> modify $ backToNormal . setMessage ("Save failed: " ++ err)

saveToFile :: FilePath -> Song -> IO (Either String ())
saveToFile path song =
    (Right <$> exportMidi path song)
        `catch` \(e :: SomeException) -> return (Left (show e))

isSuffixOf' :: String -> String -> Bool
isSuffixOf' suffix s = drop (length s - length suffix) s == suffix

startPlay :: EventM ResourceName AppState ()
startPlay = do
    st <- get
    case asPlaybackStatus st of
        Playing -> modify $ backToNormal . setMessage "Already playing"
        Paused -> do
            case asPlaybackEnv st of
                Just env -> do
                    liftIO $ resumePlayback env (asCurrentMeasure st)
                    modify $ backToNormal . setMessage "Playing..." . \s -> s{asPlaybackStatus = Playing}
                Nothing -> modify $ backToNormal . setMessage "No audio engine"
        Stopped -> do
            env <- case asPlaybackEnv st of
                Just env -> return (Just env)
                Nothing -> do
                    let bChan = asBChan st
                    liftIO $ tryInitPlayback bChan
            case env of
                Nothing -> modify $ backToNormal . setMessage "Set TERMTAB_SOUNDFONT to play"
                Just pEnv -> do
                    modify $ \s -> s{asPlaybackEnv = Just pEnv}
                    liftIO $ startPlayback pEnv (asSong st) (asCurrentMeasure st)
                    modify $ backToNormal . setMessage "Playing..." . \s -> s{asPlaybackStatus = Playing}

tryInitPlayback :: BChan AppEvent -> IO (Maybe PlaybackEnv)
tryInitPlayback bChan = do
    mPath <- lookupEnv "TERMTAB_SOUNDFONT"
    case mPath of
        Nothing -> return Nothing
        Just path -> do
            let cfg = AudioConfig{acSoundFontPath = path}
                notify = writeBChan bChan PlaybackTick
            (Just <$> initPlaybackEnv cfg notify)
                `catch` \(_ :: SomeException) -> return Nothing

-- | Play the note at the current cursor position briefly (fire-and-forget).
previewNote :: EventM ResourceName AppState ()
previewNote = do
    st <- get
    mEnv <- case asPlaybackEnv st of
        Just env -> return (Just env)
        Nothing -> do
            let bChan = asBChan st
            env <- liftIO $ tryInitPlayback bChan
            case env of
                Just e -> modify (\s -> s{asPlaybackEnv = Just e}) >> return (Just e)
                Nothing -> return Nothing
    case mEnv of
        Nothing -> return ()
        Just env -> case currentTrack st of
            Nothing -> return ()
            Just track ->
                let beats = beatsForTrackMeasure track (asCurrentMeasure st)
                    BeatIndex bIdx = asCurrentBeat st
                 in case drop bIdx beats of
                        (beat : _) ->
                            case filter (\n -> noteString n == Just (asCurrentString st)) (beatNotes beat) of
                                (note : _) -> liftIO $ void $ forkIO $ do
                                    let ch = trackChannel track
                                    engineNoteOn (peAudioEngine env) ch (notePitch note) (noteVelocity note)
                                    threadDelay 200000 -- 200ms preview
                                    engineNoteOff (peAudioEngine env) ch (notePitch note)
                                _ -> return ()
                        _ -> return ()

backToNormal :: AppState -> AppState
backToNormal st = st{asInputMode = NormalMode}

clearMessage :: AppState -> AppState
clearMessage st = st{asMessage = Nothing}

setMessage :: String -> AppState -> AppState
setMessage msg st = st{asMessage = Just msg}
