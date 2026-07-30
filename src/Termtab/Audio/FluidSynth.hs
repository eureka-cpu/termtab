{-# LANGUAGE CPP #-}

module Termtab.Audio.FluidSynth (
    FluidSynthEngine,
    initFluidSynth,
    shutdownFluidSynth,
    withFluidSynth,
    noteOn,
    noteOff,
    programChange,
) where

import Control.Exception (Exception, bracket, throwIO)
import Control.Monad (void, when)
import Foreign.C.String (withCString)
import Foreign.Ptr (Ptr, nullPtr)

import Termtab.Audio.FluidSynth.Raw
import Termtab.Audio.Types (AudioConfig (..))
import Termtab.Types (MidiChannel (..), MidiProgram (..), Pitch (..), Velocity (..))

audioDriverName :: String
#if defined(darwin_HOST_OS)
audioDriverName = "coreaudio"
#elif defined(mingw32_HOST_OS)
audioDriverName = "dsound"
#else
audioDriverName = "pulseaudio"
#endif

data FluidSynthEngine = FluidSynthEngine
    { fseSettings :: Ptr C'fluid_settings_t
    , fseSynth :: Ptr C'fluid_synth_t
    , fseDriver :: Ptr C'fluid_audio_driver_t
    }

data FluidSynthError
    = SettingsCreationFailed
    | SynthCreationFailed
    | SoundFontLoadFailed FilePath
    | AudioDriverCreationFailed
    deriving (Show)

instance Exception FluidSynthError

initFluidSynth :: AudioConfig -> IO FluidSynthEngine
initFluidSynth cfg = do
    settings <- c'new_fluid_settings
    when (settings == nullPtr) $ throwIO SettingsCreationFailed

    -- Set the audio driver explicitly per platform.
    -- FluidSynth's auto-detect often picks JACK first (even on macOS),
    -- which fails if jackd isn't running.
    withCString "audio.driver" $ \key ->
        withCString audioDriverName $ \val ->
            void $ c'fluid_settings_setstr settings key val

    synth <- c'new_fluid_synth settings
    when (synth == nullPtr) $ do
        c'delete_fluid_settings settings
        throwIO SynthCreationFailed

    sfResult <- withCString (acSoundFontPath cfg) $ \path ->
        c'fluid_synth_sfload synth path 1
    when (sfResult < 0) $ do
        c'delete_fluid_synth synth
        c'delete_fluid_settings settings
        throwIO (SoundFontLoadFailed (acSoundFontPath cfg))

    driver <- c'new_fluid_audio_driver settings synth
    when (driver == nullPtr) $ do
        c'delete_fluid_synth synth
        c'delete_fluid_settings settings
        throwIO AudioDriverCreationFailed

    return
        FluidSynthEngine
            { fseSettings = settings
            , fseSynth = synth
            , fseDriver = driver
            }

shutdownFluidSynth :: FluidSynthEngine -> IO ()
shutdownFluidSynth engine = do
    c'delete_fluid_audio_driver (fseDriver engine)
    c'delete_fluid_synth (fseSynth engine)
    c'delete_fluid_settings (fseSettings engine)

withFluidSynth :: AudioConfig -> (FluidSynthEngine -> IO a) -> IO a
withFluidSynth cfg = bracket (initFluidSynth cfg) shutdownFluidSynth

noteOn :: FluidSynthEngine -> MidiChannel -> Pitch -> Velocity -> IO ()
noteOn engine (MidiChannel ch) (Pitch p) (Velocity v) =
    void $
        c'fluid_synth_noteon
            (fseSynth engine)
            (fromIntegral ch)
            (fromIntegral p)
            (fromIntegral v)

noteOff :: FluidSynthEngine -> MidiChannel -> Pitch -> IO ()
noteOff engine (MidiChannel ch) (Pitch p) =
    void $
        c'fluid_synth_noteoff
            (fseSynth engine)
            (fromIntegral ch)
            (fromIntegral p)

programChange :: FluidSynthEngine -> MidiChannel -> MidiProgram -> IO ()
programChange engine (MidiChannel ch) (MidiProgram prog) =
    void $
        c'fluid_synth_program_change
            (fseSynth engine)
            (fromIntegral ch)
            (fromIntegral prog)
