module Termtab.Parser.MIDI (parseMidi) where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (mapMaybe)
import qualified Data.Text       as T

import qualified Codec.Midi      as MIDI

import           Termtab.Types

parseMidi :: FilePath -> IO (Either ParseError Song)
parseMidi path = do
  result <- MIDI.importFile path
  case result of
    Left  err -> return (Left (ParseErrorMalformed err))
    Right mf  -> return (Right (convertMidi mf))

convertMidi :: MIDI.Midi -> Song
convertMidi mf = Song
  { songTitle    = ""
  , songArtist   = ""
  , songTempo    = tempoFromTracks (MIDI.tracks mf)
  , songTracks   = zipWith convertTrack [0 ..] (MIDI.tracks mf)
  , songMeasures = []
  }

tempoFromTracks :: [MIDI.Track t] -> Tempo
tempoFromTracks ts =
  case [Tempo (60000000 `div` us) | (_, MIDI.TempoChange us) <- concat ts] of
    (t : _) -> t
    []      -> Tempo 120

convertTrack :: Int -> MIDI.Track t -> Track
convertTrack idx events = Track
  { trackName       = "Track " <> T.pack (show idx)
  , trackInstrument = Standard (MidiProgram 0)
  , trackChannel    = MidiChannel 0
  , trackBeats      = Map.fromList [(MeasureIndex 0, beatsFromEvents events)]
  }

beatsFromEvents :: MIDI.Track t -> [Beat]
beatsFromEvents = map singleNoteBeat . mapMaybe toNote

toNote :: (t, MIDI.Message) -> Maybe Note
toNote (_, MIDI.NoteOn _ k v) | v > 0 = Just Note
  { notePitch    = Pitch k
  , noteVelocity = Velocity v
  , noteEffects  = []
  , noteString   = Nothing
  , noteFret     = Nothing
  }
toNote _ = Nothing

singleNoteBeat :: Note -> Beat
singleNoteBeat note = Beat
  { beatDuration = Quarter
  , beatNotes    = [note]
  , beatIsRest   = False
  }
