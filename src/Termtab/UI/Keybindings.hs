module Termtab.UI.Keybindings (handleEvent) where

import Brick
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent.STM (readTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import System.Environment (lookupEnv)

import Termtab.Audio.PlaybackThread
import Termtab.Audio.Types (AudioConfig (..), PlaybackStatus (..))
import Termtab.UI.Types

handleEvent :: BrickEvent ResourceName AppEvent -> EventM ResourceName AppState ()
handleEvent (VtyEvent (V.EvKey key _mods)) = do
    mode <- gets asInputMode
    case mode of
        NormalMode -> handleNormal key
        GoToMode -> handleGoTo key
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

handleNormal :: V.Key -> EventM ResourceName AppState ()
handleNormal key = case key of
    V.KEsc -> handleEsc
    V.KChar 'h' -> modify $ clearMessage . moveBeatLeft
    V.KChar 'l' -> modify $ clearMessage . moveBeatRight
    V.KChar 'k' -> modify $ clearMessage . moveStringUp
    V.KChar 'j' -> modify $ clearMessage . moveStringDown
    V.KChar 'W' -> modify $ clearMessage . moveMeasureForward
    V.KChar 'B' -> modify $ clearMessage . moveMeasureBack
    V.KChar 'g' -> modify $ \st -> st{asInputMode = GoToMode, asMessage = Just "g-"}
    V.KChar 't' -> modify $ clearMessage . cycleDisplayMode
    V.KChar '+' -> modify $ clearMessage . zoomIn
    V.KChar '-' -> modify $ clearMessage . zoomOut
    V.KChar ':' -> modify $ \st -> st{asInputMode = CommandMode "", asMessage = Nothing}
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
    _ -> modify $ backToNormal . setMessage ("Unknown command: " ++ cmd)

startPlay :: EventM ResourceName AppState ()
startPlay = do
    st <- get
    case asPlaybackStatus st of
        Playing -> modify $ backToNormal . setMessage "Already playing"
        Paused -> do
            -- Resume from paused position
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

backToNormal :: AppState -> AppState
backToNormal st = st{asInputMode = NormalMode}

clearMessage :: AppState -> AppState
clearMessage st = st{asMessage = Nothing}

setMessage :: String -> AppState -> AppState
setMessage msg st = st{asMessage = Just msg}
