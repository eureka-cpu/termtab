module Termtab.UI (runUI) where

import Brick
import Brick.BChan (newBChan, writeBChan)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform (mkVty)

import Termtab.Audio.PlaybackThread (destroyPlaybackEnv)
import Termtab.Graphics.Detect (detectProtocol)
import Termtab.Graphics.Font (GlyphFont, bravuraFontPath, closeGlyphFont, openGlyphFont)
import Termtab.Graphics.TermColor (foregroundOrDefault)
import Termtab.Types (Song (..), TrackIndex (..))
import Termtab.UI.Keybindings (handleEvent)
import Termtab.UI.NotationGraphics (syncNotationGraphics)
import Termtab.UI.Types
import Termtab.UI.Widgets.StatusBar (commandModeAttr, renderStatusBar, statusBarAttr)
import Termtab.UI.Widgets.Tablature (barLineAttr, cursorAttr, playheadAttr, selectionAttr, stringLabelAttr)
import Termtab.UI.Widgets.TrackPanel (focusedTrackAttr, renderTrackPanel)

app :: App AppState AppEvent ResourceName
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , -- After handling an event, (re)blit the notation images so they track
          -- the current content and layout.
          appHandleEvent = \e -> handleEvent e >> syncNotationGraphics
        , -- Extents don't exist until after the first draw, so defer the initial
          -- blit to a self-posted refresh event.
          appStartEvent = do
            st <- get
            liftIO (writeBChan (asBChan st) GraphicsRefresh)
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
        , (selectionAttr, V.defAttr `V.withBackColor` V.blue `V.withForeColor` V.white)
        , (barLineAttr, V.defAttr `V.withForeColor` V.brightBlack)
        , (stringLabelAttr, V.defAttr `V.withForeColor` V.cyan)
        , (focusedTrackAttr, V.defAttr `V.withStyle` V.bold)
        , (statusBarAttr, V.defAttr `V.withStyle` V.reverseVideo)
        , (commandModeAttr, V.defAttr `V.withForeColor` V.yellow)
        ]

runUI :: Maybe FilePath -> Song -> IO ()
runUI mPath song = do
    bChan <- newBChan 10
    -- Resolve graphics before vty grabs the terminal: the foreground-color query
    -- reads stdin, which would otherwise race vty's input thread.
    protocol <- detectProtocol
    ink <- foregroundOrDefault
    mFont <- loadBravura
    let initialState =
            (initAppState mPath song bChan)
                { asProtocol = protocol
                , asInkColor = ink
                , asGlyphFont = mFont
                }
    let buildVty = mkVty V.defaultConfig
    initialVty <- buildVty
    finalState <- customMain initialVty buildVty (Just bChan) app initialState
    maybe (return ()) closeGlyphFont (asGlyphFont finalState)
    -- Clean up audio engine if it was initialized
    case asPlaybackEnv finalState of
        Just env -> destroyPlaybackEnv env
        Nothing -> return ()

-- | Load the Bravura font, tolerating a missing/unreadable path.
loadBravura :: IO (Maybe GlyphFont)
loadBravura = do
    mPath <- bravuraFontPath
    case mPath of
        Nothing -> return Nothing
        Just path -> do
            result <- try (openGlyphFont path) :: IO (Either SomeException GlyphFont)
            return (either (const Nothing) Just result)
