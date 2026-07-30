module Termtab.Audio (
    AudioEngine,
    startAudioEngine,
    stopAudioEngine,
    withAudioEngine,
    -- Direct synchronous calls (for console playback)
    engineNoteOn,
    engineNoteOff,
    engineProgramChange,
    -- Re-exports
    module Termtab.Audio.Types,
) where

import Control.Exception (bracket)

import Termtab.Audio.FluidSynth qualified as FS
import Termtab.Audio.Types
import Termtab.Types (MidiChannel, MidiProgram, Pitch, Velocity)

newtype AudioEngine = AudioEngine
    { aeBackend :: BackendHandle
    }

newtype BackendHandle = FluidSynthHandle FS.FluidSynthEngine

startAudioEngine :: BackendType -> AudioConfig -> IO AudioEngine
startAudioEngine BackendFluidSynth cfg = do
    engine <- FS.initFluidSynth cfg
    return AudioEngine{aeBackend = FluidSynthHandle engine}
startAudioEngine BackendPortMidi _ =
    error "PortMidi backend not yet implemented"

stopAudioEngine :: AudioEngine -> IO ()
stopAudioEngine engine =
    case aeBackend engine of
        FluidSynthHandle fs -> FS.shutdownFluidSynth fs

withAudioEngine :: BackendType -> AudioConfig -> (AudioEngine -> IO a) -> IO a
withAudioEngine backend cfg =
    bracket (startAudioEngine backend cfg) stopAudioEngine

-- | Send a note-on directly to the audio backend (synchronous).
engineNoteOn :: AudioEngine -> MidiChannel -> Pitch -> Velocity -> IO ()
engineNoteOn engine = case aeBackend engine of
    FluidSynthHandle fs -> FS.noteOn fs

-- | Send a note-off directly to the audio backend (synchronous).
engineNoteOff :: AudioEngine -> MidiChannel -> Pitch -> IO ()
engineNoteOff engine = case aeBackend engine of
    FluidSynthHandle fs -> FS.noteOff fs

-- | Send a program change directly to the audio backend (synchronous).
engineProgramChange :: AudioEngine -> MidiChannel -> MidiProgram -> IO ()
engineProgramChange engine = case aeBackend engine of
    FluidSynthHandle fs -> FS.programChange fs
