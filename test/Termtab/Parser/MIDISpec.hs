module Termtab.Parser.MIDISpec (tests) where

import           Control.Exception       (bracket)
import qualified Data.ByteString         as BS
import qualified Data.Map.Strict         as Map
import           System.Directory        (getTemporaryDirectory, removeFile)
import           System.IO               (hClose, hSetBinaryMode, openTempFile)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Termtab.Parser.MIDI     (parseMidi)
import           Termtab.Types

tests :: TestTree
tests = testGroup "MIDI Parser"
  [ testCase "parses C major scale: 1 track, 8 notes" $
      withMidiFixture $ \path -> do
        result <- parseMidi path
        case result of
          Left err   -> assertFailure ("parse failed: " <> show err)
          Right song -> do
            length (songTracks song) @?= 1
            totalNotes song          @?= 8

  , testCase "extracts 120 BPM tempo from SetTempo event" $
      withMidiFixture $ \path -> do
        result <- parseMidi path
        case result of
          Left err   -> assertFailure ("parse failed: " <> show err)
          Right song -> songTempo song @?= Tempo 120
  ]

totalNotes :: Song -> Int
totalNotes song =
  sum [ length (beatNotes b)
      | t  <- songTracks song
      , bs <- Map.elems (trackBeats t)
      , b  <- bs
      ]

withMidiFixture :: (FilePath -> IO a) -> IO a
withMidiFixture action = do
  tmpDir <- getTemporaryDirectory
  bracket
    (do (path, h) <- openTempFile tmpDir "termtab-test-.mid"
        hSetBinaryMode h True
        BS.hPut h cMajorScaleMidi
        hClose h
        return path)
    removeFile
    action

-- Type 0 MIDI file: C major scale (C4-C5), 120 BPM, 480 ticks/quarter note.
-- Each note is one quarter note duration (480 ticks).
cMajorScaleMidi :: BS.ByteString
cMajorScaleMidi = BS.pack
  -- MThd header
  [ 0x4D, 0x54, 0x68, 0x64   -- "MThd"
  , 0x00, 0x00, 0x00, 0x06   -- length = 6
  , 0x00, 0x00               -- format 0
  , 0x00, 0x01               -- 1 track
  , 0x01, 0xE0               -- 480 ticks per quarter note
  -- MTrk chunk
  , 0x4D, 0x54, 0x72, 0x6B   -- "MTrk"
  , 0x00, 0x00, 0x00, 0x53   -- length = 83
  -- SetTempo: 500000 us = 120 BPM
  , 0x00, 0xFF, 0x51, 0x03, 0x07, 0xA1, 0x20
  -- C4 = 60 = 0x3C
  , 0x00, 0x90, 0x3C, 0x40   -- NoteOn  ch=0 C4 vel=64
  , 0x83, 0x60, 0x80, 0x3C, 0x00   -- NoteOff ch=0 C4 (delta=480)
  -- D4 = 62 = 0x3E
  , 0x00, 0x90, 0x3E, 0x40
  , 0x83, 0x60, 0x80, 0x3E, 0x00
  -- E4 = 64 = 0x40
  , 0x00, 0x90, 0x40, 0x40
  , 0x83, 0x60, 0x80, 0x40, 0x00
  -- F4 = 65 = 0x41
  , 0x00, 0x90, 0x41, 0x40
  , 0x83, 0x60, 0x80, 0x41, 0x00
  -- G4 = 67 = 0x43
  , 0x00, 0x90, 0x43, 0x40
  , 0x83, 0x60, 0x80, 0x43, 0x00
  -- A4 = 69 = 0x45
  , 0x00, 0x90, 0x45, 0x40
  , 0x83, 0x60, 0x80, 0x45, 0x00
  -- B4 = 71 = 0x47
  , 0x00, 0x90, 0x47, 0x40
  , 0x83, 0x60, 0x80, 0x47, 0x00
  -- C5 = 72 = 0x48
  , 0x00, 0x90, 0x48, 0x40
  , 0x83, 0x60, 0x80, 0x48, 0x00
  -- EndOfTrack
  , 0x00, 0xFF, 0x2F, 0x00
  ]
