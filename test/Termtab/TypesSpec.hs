module Termtab.TypesSpec (tests) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Termtab.Types

tests :: TestTree
tests = testGroup "Types"
  [ testCase "MeasureIndex equality" $
      MeasureIndex 1 @?= MeasureIndex 1

  , testCase "MeasureIndex ordering" $
      (MeasureIndex 0 < MeasureIndex 1) @?= True

  , testCase "Pitch MIDI number preserved" $
      let Pitch n = Pitch 60 in n @?= 60

  , testCase "Tempo BPM preserved" $
      let Tempo bpm = Tempo 120 in bpm @?= 120

  , testCase "Velocity ordering" $
      (Velocity 0 < Velocity 127) @?= True

  , testCase "Duration constructors distinct" $
      (Quarter == Eighth) @?= False

  , testCase "Dotted wraps duration" $
      Dotted Quarter @?= Dotted Quarter
  ]
