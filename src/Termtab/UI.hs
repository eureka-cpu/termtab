module Termtab.UI (runUI) where

import Brick
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)

import Termtab.Types (Song (..), TrackIndex (..))
import Termtab.UI.Keybindings (handleEvent)
import Termtab.UI.Types
import Termtab.UI.Widgets.StatusBar (commandModeAttr, renderStatusBar, statusBarAttr)
import Termtab.UI.Widgets.Tablature (barLineAttr, cursorAttr, stringLabelAttr)
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
        , (barLineAttr, V.defAttr `V.withForeColor` V.brightBlack)
        , (stringLabelAttr, V.defAttr `V.withForeColor` V.cyan)
        , (focusedTrackAttr, V.defAttr `V.withStyle` V.bold)
        , (statusBarAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (commandModeAttr, V.defAttr `V.withForeColor` V.yellow)
        ]

runUI :: Maybe FilePath -> Song -> IO ()
runUI mPath song = do
    let initialState = initAppState mPath song
    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    _ <- customMain initialVty buildVty Nothing app initialState
    return ()
