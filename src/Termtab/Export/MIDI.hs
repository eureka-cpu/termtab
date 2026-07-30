module Termtab.Export.MIDI (exportMidi) where

import Codec.Midi qualified as MIDI
import Data.Map.Strict qualified as Map
import Termtab.Types

-- | Export a Song to a MIDI file.
exportMidi :: FilePath -> Song -> IO ()
exportMidi path song =
    MIDI.exportFile path (songToMidi song)

songToMidi :: Song -> MIDI.Midi
songToMidi song =
    MIDI.Midi
        { MIDI.fileType = MIDI.MultiTrack
        , MIDI.timeDiv = MIDI.TicksPerBeat ticksPerQuarter
        , MIDI.tracks = tempoTrack song : map (trackToMidi song) (songTracks song)
        }

ticksPerQuarter :: Int
ticksPerQuarter = 480

tempoTrack :: Song -> MIDI.Track MIDI.Ticks
tempoTrack song =
    let Tempo bpm = songTempo song
        microsecondsPerBeat = 60000000 `div` bpm
     in [(0, MIDI.TempoChange microsecondsPerBeat), (0, MIDI.TrackEnd)]

trackToMidi :: Song -> Track -> MIDI.Track MIDI.Ticks
trackToMidi song track =
    let measures = songMeasures song
        events = concatMap (measureToEvents track) (zip [0 ..] measures)
     in events ++ [(0, MIDI.TrackEnd)]

measureToEvents :: Track -> (Int, Measure) -> [(MIDI.Ticks, MIDI.Message)]
measureToEvents track (mi, _measure) =
    let beats = maybe [] id (Map.lookup (MeasureIndex mi) (trackBeats track))
        MidiChannel ch = trackChannel track
     in concatMap (beatToEvents ch) beats

beatToEvents :: Int -> Beat -> [(MIDI.Ticks, MIDI.Message)]
beatToEvents ch beat
    | beatIsRest beat = [(durationToTicks (beatDuration beat), MIDI.NoteOff ch 0 0)]
    | null (beatNotes beat) = [(durationToTicks (beatDuration beat), MIDI.NoteOff ch 0 0)]
    | otherwise =
        let dur = durationToTicks (beatDuration beat)
            noteOns =
                [ (0, MIDI.NoteOn ch (p `mod` 128) (v `mod` 128))
                | n <- beatNotes beat
                , let Pitch p = notePitch n
                , let Velocity v = noteVelocity n
                ]
            noteOffs =
                [ (0, MIDI.NoteOff ch (p `mod` 128) 0)
                | n <- beatNotes beat
                , let Pitch p = notePitch n
                ]
         in case (noteOns, noteOffs) of
                ([], _) -> [(dur, MIDI.NoteOff ch 0 0)]
                (_, []) -> noteOns ++ [(dur, MIDI.NoteOff ch 0 0)]
                (_, (_, firstOff) : restOffs) ->
                    noteOns ++ [(dur, firstOff)] ++ restOffs

durationToTicks :: Duration -> Int
durationToTicks Whole = ticksPerQuarter * 4
durationToTicks Half = ticksPerQuarter * 2
durationToTicks Quarter = ticksPerQuarter
durationToTicks Eighth = ticksPerQuarter `div` 2
durationToTicks Sixteenth = ticksPerQuarter `div` 4
durationToTicks Thirty2nd = ticksPerQuarter `div` 8
durationToTicks (Dotted d) = durationToTicks d + durationToTicks d `div` 2
durationToTicks (Triplet d) = durationToTicks d * 2 `div` 3
