module Termtab.Audio.PlaybackSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Audio.Playback (durationToMicroseconds)
import Termtab.Types

tests :: TestTree
tests =
    testGroup
        "Audio.Playback"
        [ testCase "Quarter note at 120 BPM = 500ms" $
            durationToMicroseconds Quarter (Tempo 120) @?= 500000
        , testCase "Eighth note at 120 BPM = 250ms" $
            durationToMicroseconds Eighth (Tempo 120) @?= 250000
        , testCase "Half note at 120 BPM = 1000ms" $
            durationToMicroseconds Half (Tempo 120) @?= 1000000
        , testCase "Whole note at 120 BPM = 2000ms" $
            durationToMicroseconds Whole (Tempo 120) @?= 2000000
        , testCase "Sixteenth note at 120 BPM = 125ms" $
            durationToMicroseconds Sixteenth (Tempo 120) @?= 125000
        , testCase "32nd note at 120 BPM = 62.5ms" $
            durationToMicroseconds Thirty2nd (Tempo 120) @?= 62500
        , testCase "Dotted quarter = 1.5x quarter" $
            durationToMicroseconds (Dotted Quarter) (Tempo 120) @?= 750000
        , testCase "Triplet quarter = 2/3 quarter" $
            durationToMicroseconds (Triplet Quarter) (Tempo 120) @?= 333333
        , testCase "Quarter note at 60 BPM = 1000ms" $
            durationToMicroseconds Quarter (Tempo 60) @?= 1000000
        , testCase "Quarter note at 240 BPM = 250ms" $
            durationToMicroseconds Quarter (Tempo 240) @?= 250000
        ]
