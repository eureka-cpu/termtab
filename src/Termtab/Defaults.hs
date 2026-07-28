module Termtab.Defaults (defaultSong) where

import qualified Data.Map.Strict as Map

import           Termtab.Types

-- Standard 6-string guitar tuning, low to high: E2 A2 D3 G3 B3 E4
standardTuning :: [Pitch]
standardTuning = map Pitch [40, 45, 50, 55, 59, 64]

defaultSong :: Song
defaultSong = Song
  { songTitle    = "Untitled"
  , songArtist   = ""
  , songTempo    = Tempo 120
  , songTracks   = [defaultTrack]
  , songMeasures = [defaultMeasure]
  }

defaultTrack :: Track
defaultTrack = Track
  { trackName       = "Guitar"
  , trackInstrument = Guitar { tuning = standardTuning, stringCount = 6 }
  , trackChannel    = MidiChannel 0
  , trackBeats      = Map.empty
  }

defaultMeasure :: Measure
defaultMeasure = Measure
  { measureIndex  = MeasureIndex 0
  , timeSignature = TimeSignature 4 4
  , keySignature  = KeySignature 0 Major
  , tempoChange   = Nothing
  }
