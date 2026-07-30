module Termtab.UI (runUI) where

import Brick
import Brick.BChan (newBChan)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)

import Termtab.Audio.PlaybackThread (destroyPlaybackEnv)
import Termtab.Types (Song (..), TrackIndex (..))
import Termtab.UI.Keybindings (handleEvent)
import Termtab.UI.Types
import Termtab.UI.Widgets.StatusBar (commandModeAttr, renderStatusBar, statusBarAttr)
import Termtab.UI.Widgets.Tablature (barLineAttr, cursorAttr, playheadAttr, stringLabelAttr)
import Termtab.UI.Widgets.TrackPanel (focusedTrackAttr, renderTrackPanel)

app :: App AppState AppEvent ResourceName
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = handleEvent
        , appStartEvent = return ()
        , appAttrMap = const theAttrMap
        }

drawUI :: AppState -> [Widget ResourceName]
drawUI st =
    [ vBox $
        [ renderTrackPanel st (TrackIndex i) track
        | (i, track) <- zip [0 ..] (songTracks (asSong st))
        ]
            ++ [renderStatusBar st]
    ]

theAttrMap :: AttrMap
theAttrMap =
    attrMap
        V.defAttr
        [ (cursorAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (playheadAttr, V.defAttr `V.withBackColor` V.green `V.withForeColor` V.black)
        , (barLineAttr, V.defAttr `V.withForeColor` V.brightBlack)
        , (stringLabelAttr, V.defAttr `V.withForeColor` V.cyan)
        , (focusedTrackAttr, V.defAttr `V.withStyle` V.bold)
        , (statusBarAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (commandModeAttr, V.defAttr `V.withForeColor` V.yellow)
        ]

runUI :: Maybe FilePath -> Song -> IO ()
runUI mPath song = do
    bChan <- newBChan 10
    let initialState = initAppState mPath song bChan
    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty (Just bChan) app initialState
    -- Clean up audio engine if it was initialized
    case asPlaybackEnv finalState of
        Just env -> destroyPlaybackEnv env
        Nothing -> return ()
