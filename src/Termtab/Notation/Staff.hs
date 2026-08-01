{- | Pure music-notation helpers shared by the text staff widget
("Termtab.UI.Widgets.Notation") and the rasterized image renderer
("Termtab.Graphics.Notation"): mapping pitches to treble-clef staff positions
and durations to SMuFL glyph code points.

Staff positions are integers where 0 = E4 (bottom line) and 8 = F5 (top line);
even positions are lines, odd positions are spaces. Positions below 0 / above 8
are ledger territory.
-}
module Termtab.Notation.Staff (
    pitchToStaffPos,
    isStaffLine,
    noteheadGlyph,
    restGlyph,
    gClefGlyph,
) where

import Termtab.Types

-- | Is this staff position a line (as opposed to a space)?
isStaffLine :: Int -> Bool
isStaffLine pos = even pos && pos >= 0 && pos <= 8

{- | Convert a MIDI pitch to a treble-clef staff position.
Guitar/bass are transposed up an octave (standard tab convention).
Position 0 = E4 (bottom line), 8 = F5 (top line).
-}
pitchToStaffPos :: Instrument -> Pitch -> Int
pitchToStaffPos instr (Pitch midi) =
    let transposed = case instr of
            Guitar _ _ -> midi + 12
            Bass _ _ -> midi + 12
            Standard _ -> midi
        octave = transposed `div` 12 - 1
        noteInOctave = transposed `mod` 12
        naturalPos = case noteInOctave of
            0 -> 0
            1 -> 0
            2 -> 1
            3 -> 1
            4 -> 2
            5 -> 3
            6 -> 3
            7 -> 4
            8 -> 4
            9 -> 5
            10 -> 5
            11 -> 6
            _ -> 0
        posFromC4 = (octave - 4) * 7 + naturalPos
     in posFromC4 - 2

-- | SMuFL notehead code point for a duration.
noteheadGlyph :: Duration -> Char
noteheadGlyph Whole = '\xE0A2' -- noteheadWhole
noteheadGlyph Half = '\xE0A3' -- noteheadHalf
noteheadGlyph (Dotted d) = noteheadGlyph d
noteheadGlyph (Triplet d) = noteheadGlyph d
noteheadGlyph _ = '\xE0A4' -- noteheadBlack

-- | SMuFL rest code point for a duration.
restGlyph :: Duration -> Char
restGlyph Whole = '\xE4E3' -- restWhole
restGlyph Half = '\xE4E4' -- restHalf
restGlyph Quarter = '\xE4E5' -- restQuarter
restGlyph Eighth = '\xE4E6' -- rest8th
restGlyph Sixteenth = '\xE4E7' -- rest16th
restGlyph Thirty2nd = '\xE4E8' -- rest32nd
restGlyph (Dotted d) = restGlyph d
restGlyph (Triplet d) = restGlyph d

-- | SMuFL G (treble) clef.
gClefGlyph :: Char
gClefGlyph = '\xE050'
