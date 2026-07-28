{-# LANGUAGE DuplicateRecordFields #-}
module Termtab.Types
  ( MeasureIndex(..)
  , BeatIndex(..)
  , TrackIndex(..)
  , StringIndex(..)
  , FretNumber(..)
  , MidiChannel(..)
  , MidiProgram(..)
  , Velocity(..)
  , Tempo(..)
  , Pitch(..)
  , Duration(..)
  , Mode(..)
  , TimeSignature(..)
  , KeySignature(..)
  , BendValue(..)
  , SlideType(..)
  , NoteEffect(..)
  , Note(..)
  , Beat(..)
  , Measure(..)
  , Instrument(..)
  , Track(..)
  , Song(..)
  , UnsupportedFormat(..)
  , ParseError(..)
  ) where

import Data.Map.Strict (Map)
import Data.Text       (Text)

newtype MeasureIndex = MeasureIndex Int deriving (Show, Eq, Ord)
newtype BeatIndex    = BeatIndex Int    deriving (Show, Eq, Ord)
newtype TrackIndex   = TrackIndex Int   deriving (Show, Eq, Ord)
newtype StringIndex  = StringIndex Int  deriving (Show, Eq, Ord)
newtype FretNumber   = FretNumber Int   deriving (Show, Eq, Ord)
newtype MidiChannel  = MidiChannel Int  deriving (Show, Eq, Ord)
newtype MidiProgram  = MidiProgram Int  deriving (Show, Eq, Ord)
newtype Velocity     = Velocity Int     deriving (Show, Eq, Ord)
newtype Tempo        = Tempo Int        deriving (Show, Eq, Ord)
newtype Pitch        = Pitch Int        deriving (Show, Eq, Ord)

data Duration
  = Whole
  | Half
  | Quarter
  | Eighth
  | Sixteenth
  | Thirty2nd
  | Dotted Duration
  | Triplet Duration
  deriving (Show, Eq, Ord)

data Mode = Major | Minor deriving (Show, Eq)

data TimeSignature = TimeSignature Int Int
  deriving (Show, Eq)

data KeySignature = KeySignature Int Mode
  deriving (Show, Eq)

newtype BendValue = BendValue Int deriving (Show, Eq)

data SlideType
  = SlideIntoFrom
  | SlideIntoBelow
  | SlideOutDown
  | SlideOutUp
  | ShiftSlide
  | LegatoSlide
  deriving (Show, Eq)

data NoteEffect
  = Bend BendValue
  | Slide SlideType
  | HammerOn
  | PullOff
  | Vibrato
  | PalmMute
  deriving (Show, Eq)

data Note = Note
  { notePitch    :: Pitch
  , noteVelocity :: Velocity
  , noteEffects  :: [NoteEffect]
  , noteString   :: Maybe StringIndex
  , noteFret     :: Maybe FretNumber
  } deriving (Show, Eq)

data Beat = Beat
  { beatDuration :: Duration
  , beatNotes    :: [Note]
  , beatIsRest   :: Bool
  } deriving (Show, Eq)

data Measure = Measure
  { measureIndex  :: MeasureIndex
  , timeSignature :: TimeSignature
  , keySignature  :: KeySignature
  , tempoChange   :: Maybe Tempo
  } deriving (Show, Eq)

data Instrument
  = Guitar { tuning :: [Pitch], stringCount :: Int }
  | Bass   { tuning :: [Pitch], stringCount :: Int }
  | Standard MidiProgram
  deriving (Show, Eq)

data Track = Track
  { trackName       :: Text
  , trackInstrument :: Instrument
  , trackChannel    :: MidiChannel
  , trackBeats      :: Map MeasureIndex [Beat]
  } deriving (Show, Eq)

data Song = Song
  { songTitle    :: Text
  , songArtist   :: Text
  , songTempo    :: Tempo
  , songTracks   :: [Track]
  , songMeasures :: [Measure]
  } deriving (Show, Eq)

data UnsupportedFormat = UnsupportedFormat
  { unsupportedPath   :: FilePath
  , unsupportedReason :: Text
  } deriving (Show, Eq)

data ParseError
  = ParseErrorUnsupported UnsupportedFormat
  | ParseErrorMalformed   String
  deriving (Show)
