module Termtab.UI.Widgets.TrackPanel (renderTrackPanel, focusedTrackAttr) where

import Brick
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

import Termtab.Types
import Termtab.UI.Types
import Termtab.UI.Widgets.Notation (renderNotation)
import Termtab.UI.Widgets.Tablature (renderTablature)

focusedTrackAttr :: AttrName
focusedTrackAttr = attrName "focusedTrack"

renderTrackPanel :: AppState -> TrackIndex -> Track -> Widget ResourceName
renderTrackPanel st tIdx track =
    let TrackIndex i = tIdx
        label = "Track " ++ show (i + 1) ++ ": " ++ T.unpack (trackName track)
        isFocused = tIdx == asCurrentTrack st
        header =
            if isFocused
                then withAttr focusedTrackAttr (str label)
                else str label
        mode = Map.findWithDefault (defaultModeFor track) tIdx (asDisplayModes st)
        effectiveMode = case trackInstrument track of
            Standard _ -> NotationOnly
            _ -> mode
        content = case effectiveMode of
            TabOnly -> renderTablature st tIdx
            NotationOnly -> renderNotation st tIdx
            TabAndNotation ->
                vBox
                    [ renderNotation st tIdx
                    , renderTablature st tIdx
                    ]
     in vBox [header, content]

defaultModeFor :: Track -> DisplayMode
defaultModeFor track = case trackInstrument track of
    Guitar _ _ -> TabOnly
    Bass _ _ -> TabOnly
    Standard _ -> NotationOnly
