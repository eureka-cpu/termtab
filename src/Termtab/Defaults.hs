module Termtab.Defaults (defaultSong, defaultMeasureBeats) where

import Data.Map.Strict qualified as Map

import Termtab.Types

-- Standard 6-string guitar tuning, low to high: E2 A2 D3 G3 B3 E4
standardTuning :: [Pitch]
standardTuning = map Pitch [40, 45, 50, 55, 59, 64]

defaultSong :: Song
defaultSong =
    Song
        { songTitle = "Untitled"
        , songArtist = ""
        , songTempo = Tempo 120
        , songTracks = [defaultTrack]
        , songMeasures = [defaultMeasure]
        }

defaultTrack :: Track
defaultTrack =
    Track
        { trackName = "Guitar"
        , trackInstrument = Guitar{tuning = standardTuning, stringCount = 6}
        , trackChannel = MidiChannel 0
        , trackBeats =
            Map.singleton (MeasureIndex 0) $
                [ mkBeat Quarter [(5, 0, 40), (4, 2, 47)] -- E2 + B2
                , mkBeat Quarter [(3, 2, 52)] -- E3
                , mkBeat Quarter [(2, 0, 55)] -- G3
                , mkBeat Quarter [(1, 3, 62), (0, 0, 64)] -- D4 + E4
                ]
        }

mkBeat :: Duration -> [(Int, Int, Int)] -> Beat
mkBeat dur notes =
    Beat
        { beatDuration = dur
        , beatNotes =
            [ Note
                { notePitch = Pitch pitch
                , noteVelocity = Velocity 100
                , noteEffects = []
                , noteString = Just (StringIndex s)
                , noteFret = Just (FretNumber f)
                }
            | (s, f, pitch) <- notes
            ]
        , beatIsRest = False
        }

-- | Generate the initial beat list for a time signature (all quarter rests).
defaultMeasureBeats :: TimeSignature -> [Beat]
defaultMeasureBeats (TimeSignature num _) =
    replicate num Beat{beatDuration = Quarter, beatNotes = [], beatIsRest = True}

defaultMeasure :: Measure
defaultMeasure =
    Measure
        { measureIndex = MeasureIndex 0
        , timeSignature = TimeSignature 4 4
        , keySignature = KeySignature 0 Major
        , tempoChange = Nothing
        }
