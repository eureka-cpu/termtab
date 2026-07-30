module Termtab.UI.Widgets.StatusBar (renderStatusBar, statusBarAttr, commandModeAttr) where

import Brick
import System.FilePath (takeFileName)

import Termtab.Audio.Types (PlaybackStatus (..))
import Termtab.Types
import Termtab.UI.Types

statusBarAttr :: AttrName
statusBarAttr = attrName "statusBar"

commandModeAttr :: AttrName
commandModeAttr = attrName "commandMode"

renderStatusBar :: AppState -> Widget ResourceName
renderStatusBar st = case asInputMode st of
    CommandMode buf ->
        withAttr commandModeAttr $
            str (":" ++ buf)
    _ ->
        withAttr statusBarAttr $
            hBox
                [ fileInfo
                , fill ' '
                , tempoInfo
                , fill ' '
                , positionInfo
                , playbackInfo
                , case asMessage st of
                    Just msg -> str ("  " ++ msg)
                    Nothing -> emptyWidget
                ]
  where
    fileInfo =
        let modified = if null (asUndoStack st) then "" else " [modified]"
         in str $ case asFilePath st of
                Just fp -> "File: " ++ takeFileName fp ++ modified
                Nothing -> "[scratch]" ++ modified

    tempoInfo =
        let Tempo bpm = songTempo (asSong st)
            timeSig = case currentMeasure st of
                Just m ->
                    let TimeSignature num den = timeSignature m
                     in show num ++ "/" ++ show den
                Nothing -> "4/4"
         in str ("Tempo: " ++ show bpm ++ " BPM  " ++ timeSig)

    positionInfo =
        let MeasureIndex m = asCurrentMeasure st
            total = measureCount st
         in str ("Bar: " ++ show (m + 1) ++ "/" ++ show total ++ "  [zoom: " ++ show (asZoom st) ++ "]")

    playbackInfo = case asPlaybackStatus st of
        Playing -> str " [PLAYING]"
        Paused -> str " [PAUSED]"
        Stopped -> emptyWidget
