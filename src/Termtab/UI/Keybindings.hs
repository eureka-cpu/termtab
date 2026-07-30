module Termtab.UI.Keybindings (handleEvent) where

import Brick
import Graphics.Vty qualified as V

import Termtab.UI.Types

handleEvent :: BrickEvent ResourceName AppEvent -> EventM ResourceName AppState ()
handleEvent (VtyEvent (V.EvKey key _mods)) = do
    mode <- gets asInputMode
    case mode of
        NormalMode -> handleNormal key
        GoToMode -> handleGoTo key
        CommandMode buf -> handleCommand key buf
handleEvent _ = return ()

handleNormal :: V.Key -> EventM ResourceName AppState ()
handleNormal key = case key of
    V.KEsc -> halt
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
    _ -> modify $ backToNormal . setMessage ("Unknown command: " ++ cmd)

backToNormal :: AppState -> AppState
backToNormal st = st{asInputMode = NormalMode}

clearMessage :: AppState -> AppState
clearMessage st = st{asMessage = Nothing}

setMessage :: String -> AppState -> AppState
setMessage msg st = st{asMessage = Just msg}
