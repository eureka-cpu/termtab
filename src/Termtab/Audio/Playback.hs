module Termtab.Audio.Playback (
    durationToMicroseconds,
    playNote,
    playSequence,
) where

import Control.Concurrent (threadDelay)

import Termtab.Audio (AudioEngine, engineNoteOff, engineNoteOn)
import Termtab.Types

{- | Convert a 'Duration' at a given 'Tempo' (BPM) to microseconds.

A quarter note at 120 BPM = 500,000 µs (0.5 seconds).
-}
durationToMicroseconds :: Duration -> Tempo -> Int
durationToMicroseconds dur (Tempo bpm) =
    let quarterUs = 60_000_000 `div` bpm
     in durationMultiplier dur quarterUs

durationMultiplier :: Duration -> Int -> Int
durationMultiplier Whole base = base * 4
durationMultiplier Half base = base * 2
durationMultiplier Quarter base = base
durationMultiplier Eighth base = base `div` 2
durationMultiplier Sixteenth base = base `div` 4
durationMultiplier Thirty2nd base = base `div` 8
durationMultiplier (Dotted d) base = durationMultiplier d base + durationMultiplier d base `div` 2
durationMultiplier (Triplet d) base = durationMultiplier d base * 2 `div` 3

-- | Play a single note for the given duration, then turn it off.
playNote :: AudioEngine -> MidiChannel -> Pitch -> Velocity -> Duration -> Tempo -> IO ()
playNote engine ch pitch vel dur tempo = do
    engineNoteOn engine ch pitch vel
    threadDelay (durationToMicroseconds dur tempo)
    engineNoteOff engine ch pitch

-- | Play a sequence of beats on a given channel at a given tempo.
playSequence :: AudioEngine -> MidiChannel -> Tempo -> [Beat] -> IO ()
playSequence engine ch tempo = mapM_ playBeat
  where
    playBeat beat
        | beatIsRest beat = threadDelay (durationToMicroseconds (beatDuration beat) tempo)
        | otherwise = do
            mapM_ (\n -> engineNoteOn engine ch (notePitch n) (noteVelocity n)) (beatNotes beat)
            threadDelay (durationToMicroseconds (beatDuration beat) tempo)
            mapM_ (engineNoteOff engine ch . notePitch) (beatNotes beat)
