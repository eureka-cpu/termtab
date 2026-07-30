module Termtab.Audio.Types (
    AudioCommand (..),
    PlaybackStatus (..),
    AudioConfig (..),
    BackendType (..),
) where

import Termtab.Types (MeasureIndex, MidiChannel, MidiProgram, Pitch, Velocity)

data AudioCommand
    = CmdNoteOn MidiChannel Pitch Velocity
    | CmdNoteOff MidiChannel Pitch
    | CmdProgramChange MidiChannel MidiProgram
    | CmdPlay
    | CmdStop
    | CmdPause
    | CmdSeek MeasureIndex
    | CmdShutdown
    deriving (Show, Eq)

data PlaybackStatus
    = Playing
    | Paused
    | Stopped
    deriving (Show, Eq)

newtype AudioConfig = AudioConfig
    { acSoundFontPath :: FilePath
    }
    deriving (Show, Eq)

data BackendType
    = BackendFluidSynth
    | BackendPortMidi
    deriving (Show, Eq)
