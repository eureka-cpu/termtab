module Termtab.UI.TypesSpec (tests) where

import Brick.BChan (newBChan)
import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Defaults (defaultSong)
import Termtab.Types
import Termtab.UI.Types

mkState :: Song -> IO AppState
mkState song = do
    bChan <- newBChan 1
    return $ initAppState Nothing song bChan

tests :: TestTree
tests =
    testGroup
        "UI.Types"
        [ testGroup
            "initAppState"
            [ testCase "starts at measure 0, beat 0" $ do
                st <- mkState defaultSong
                asCurrentMeasure st @?= MeasureIndex 0
                asCurrentBeat st @?= BeatIndex 0
            , testCase "starts in NormalMode" $ do
                st <- mkState defaultSong
                asInputMode st @?= NormalMode
            , testCase "default zoom is 4" $ do
                st <- mkState defaultSong
                asZoom st @?= 4
            , testCase "guitar track defaults to TabOnly" $ do
                st <- mkState defaultSong
                Map.lookup (TrackIndex 0) (asDisplayModes st) @?= Just TabOnly
            ]
        , testGroup
            "navigation"
            [ testCase "moveBeatLeft clamps at 0" $ do
                st <- mkState defaultSong
                asCurrentBeat (moveBeatLeft st) @?= BeatIndex 0
            , testCase "moveMeasureBack clamps at 0" $ do
                st <- mkState defaultSong
                asCurrentMeasure (moveMeasureBack st) @?= MeasureIndex 0
            , testCase "moveMeasureForward resets beat to 0" $ do
                st <- mkState songWith2Measures
                let st' = st{asCurrentBeat = BeatIndex 2}
                asCurrentBeat (moveMeasureForward st') @?= BeatIndex 0
            , testCase "moveStringDown clamps at max" $ do
                st <- mkState defaultSong
                let st' = st{asCurrentString = StringIndex 5}
                asCurrentString (moveStringDown st') @?= StringIndex 5
            , testCase "moveStringUp clamps at 0" $ do
                st <- mkState defaultSong
                asCurrentString (moveStringUp st) @?= StringIndex 0
            , testCase "goToStart resets measure and beat" $ do
                st <- mkState songWith2Measures
                let st' = st{asCurrentMeasure = MeasureIndex 1, asCurrentBeat = BeatIndex 3}
                let st'' = goToStart st'
                asCurrentMeasure st'' @?= MeasureIndex 0
                asCurrentBeat st'' @?= BeatIndex 0
            ]
        , testGroup
            "display mode"
            [ testCase "cycles TabOnly -> NotationOnly -> TabAndNotation -> TabOnly" $ do
                st <- mkState defaultSong
                let st1 = cycleDisplayMode st
                Map.lookup (TrackIndex 0) (asDisplayModes st1) @?= Just NotationOnly
                let st2 = cycleDisplayMode st1
                Map.lookup (TrackIndex 0) (asDisplayModes st2) @?= Just TabAndNotation
                let st3 = cycleDisplayMode st2
                Map.lookup (TrackIndex 0) (asDisplayModes st3) @?= Just TabOnly
            ]
        , testGroup
            "zoom"
            [ testCase "zoomIn increases" $ do
                st <- mkState defaultSong
                asZoom (zoomIn st) @?= 5
            , testCase "zoomOut decreases" $ do
                st <- mkState defaultSong
                asZoom (zoomOut st) @?= 3
            , testCase "zoomIn caps at 12" $ do
                st <- mkState defaultSong
                let st' = st{asZoom = 12}
                asZoom (zoomIn st') @?= 12
            , testCase "zoomOut caps at 2" $ do
                st <- mkState defaultSong
                let st' = st{asZoom = 2}
                asZoom (zoomOut st') @?= 2
            ]
        ]

songWith2Measures :: Song
songWith2Measures =
    defaultSong
        { songMeasures =
            [ Measure (MeasureIndex 0) (TimeSignature 4 4) (KeySignature 0 Major) Nothing
            , Measure (MeasureIndex 1) (TimeSignature 4 4) (KeySignature 0 Major) Nothing
            ]
        }
