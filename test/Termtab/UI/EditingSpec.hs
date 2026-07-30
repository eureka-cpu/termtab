module Termtab.UI.EditingSpec (tests) where

import Brick.BChan (newBChan)
import Data.Map.Strict qualified as Map
import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Defaults (defaultSong)
import Termtab.Types
import Termtab.UI.Editing
import Termtab.UI.Types

mkState :: Song -> IO AppState
mkState song = do
    bChan <- newBChan 1
    return $ initAppState Nothing song bChan

-- A song with one measure and one track with some beats
songWithBeats :: Song
songWithBeats =
    defaultSong
        { songTracks =
            [ (head (songTracks defaultSong))
                { trackBeats =
                    Map.singleton
                        (MeasureIndex 0)
                        [ Beat Quarter [note0] False
                        , Beat Quarter [] True
                        ]
                }
            ]
        }
  where
    note0 =
        Note
            { notePitch = Pitch 64
            , noteVelocity = Velocity 100
            , noteEffects = []
            , noteString = Just (StringIndex 0)
            , noteFret = Just (FretNumber 0)
            }

tests :: TestTree
tests =
    testGroup
        "UI.Editing"
        [ testGroup
            "fret entry"
            [ testCase "entering fret 5 creates a note" $ do
                st <- mkState defaultSong
                let st' = enterFretDigit 5 st
                    track = head (songTracks (asSong st'))
                    beats = beatsForTrackMeasure track (MeasureIndex 0)
                length beats @?= 1
                let beat = head beats
                beatIsRest beat @?= False
                length (beatNotes beat) @?= 1
                noteFret (head (beatNotes beat)) @?= Just (FretNumber 5)
            , testCase "two-digit fret: 1 then 2 = fret 12" $ do
                st <- mkState defaultSong
                let st' = enterFretDigit 1 st
                    st'' = enterFretDigit 2 st'
                    track = head (songTracks (asSong st''))
                    beats = beatsForTrackMeasure track (MeasureIndex 0)
                    beat = head beats
                noteFret (head (beatNotes beat)) @?= Just (FretNumber 12)
            , testCase "two-digit overflow: 3 then 5 = fret 5 (35 > 24)" $ do
                st <- mkState defaultSong
                let st' = enterFretDigit 3 st
                    st'' = enterFretDigit 5 st'
                    track = head (songTracks (asSong st''))
                    beats = beatsForTrackMeasure track (MeasureIndex 0)
                    beat = head beats
                noteFret (head (beatNotes beat)) @?= Just (FretNumber 5)
            ]
        , testGroup
            "delete"
            [ testCase "deleting removes note from beat" $ do
                st <- mkState songWithBeats
                let st' = deleteNoteAtCursor st
                    track = head (songTracks (asSong st'))
                    beats = beatsForTrackMeasure track (MeasureIndex 0)
                    beat = head beats
                beatNotes beat @?= []
            ]
        , testGroup
            "rest"
            [ testCase "inserting rest sets beatIsRest and clears notes" $ do
                st <- mkState songWithBeats
                let st' = insertRest st
                    track = head (songTracks (asSong st'))
                    beats = beatsForTrackMeasure track (MeasureIndex 0)
                    beat = head beats
                beatIsRest beat @?= True
                beatNotes beat @?= []
            ]
        , testGroup
            "duration"
            [ testCase "cycle Quarter -> Eighth" $ do
                st <- mkState songWithBeats
                let st' = cycleDuration st
                    track = head (songTracks (asSong st'))
                    beats = beatsForTrackMeasure track (MeasureIndex 0)
                    beat = head beats
                beatDuration beat @?= Eighth
            ]
        , testGroup
            "undo/redo"
            [ testCase "undo restores previous song" $ do
                st <- mkState defaultSong
                let original = asSong st
                    st' = enterFretDigit 5 st
                    st'' = undo st'
                asSong st'' @?= original
            , testCase "redo re-applies" $ do
                st <- mkState defaultSong
                let st' = enterFretDigit 5 st
                    edited = asSong st'
                    st'' = undo st'
                    st''' = redo st''
                asSong st''' @?= edited
            , testCase "new edit clears redo stack" $ do
                st <- mkState defaultSong
                let st' = enterFretDigit 5 st
                    st'' = undo st'
                    st''' = enterFretDigit 3 st''
                asRedoStack st''' @?= []
            ]
        ]
