{-# LANGUAGE ForeignFunctionInterface #-}

module Termtab.Audio.FluidSynth.Raw (
    -- * Opaque C types
    C'fluid_settings_t,
    C'fluid_synth_t,
    C'fluid_audio_driver_t,

    -- * Settings
    c'new_fluid_settings,
    c'delete_fluid_settings,
    c'fluid_settings_setstr,
    c'fluid_settings_setint,

    -- * Synth
    c'new_fluid_synth,
    c'delete_fluid_synth,
    c'fluid_synth_sfload,

    -- * Audio driver
    c'new_fluid_audio_driver,
    c'delete_fluid_audio_driver,

    -- * Note playback
    c'fluid_synth_noteon,
    c'fluid_synth_noteoff,

    -- * Program change
    c'fluid_synth_program_change,
) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)

-- Opaque C types
data C'fluid_settings_t
data C'fluid_synth_t
data C'fluid_audio_driver_t

-- Settings
foreign import ccall "fluidsynth.h new_fluid_settings"
    c'new_fluid_settings :: IO (Ptr C'fluid_settings_t)

foreign import ccall "fluidsynth.h delete_fluid_settings"
    c'delete_fluid_settings :: Ptr C'fluid_settings_t -> IO ()

foreign import ccall "fluidsynth.h fluid_settings_setstr"
    c'fluid_settings_setstr :: Ptr C'fluid_settings_t -> CString -> CString -> IO CInt

foreign import ccall "fluidsynth.h fluid_settings_setint"
    c'fluid_settings_setint :: Ptr C'fluid_settings_t -> CString -> CInt -> IO CInt

-- Synth
foreign import ccall "fluidsynth.h new_fluid_synth"
    c'new_fluid_synth :: Ptr C'fluid_settings_t -> IO (Ptr C'fluid_synth_t)

foreign import ccall "fluidsynth.h delete_fluid_synth"
    c'delete_fluid_synth :: Ptr C'fluid_synth_t -> IO ()

foreign import ccall "fluidsynth.h fluid_synth_sfload"
    c'fluid_synth_sfload :: Ptr C'fluid_synth_t -> CString -> CInt -> IO CInt

-- Audio driver
foreign import ccall "fluidsynth.h new_fluid_audio_driver"
    c'new_fluid_audio_driver :: Ptr C'fluid_settings_t -> Ptr C'fluid_synth_t -> IO (Ptr C'fluid_audio_driver_t)

foreign import ccall "fluidsynth.h delete_fluid_audio_driver"
    c'delete_fluid_audio_driver :: Ptr C'fluid_audio_driver_t -> IO ()

-- Note playback
foreign import ccall "fluidsynth.h fluid_synth_noteon"
    c'fluid_synth_noteon :: Ptr C'fluid_synth_t -> CInt -> CInt -> CInt -> IO CInt

foreign import ccall "fluidsynth.h fluid_synth_noteoff"
    c'fluid_synth_noteoff :: Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt

-- Program change
foreign import ccall "fluidsynth.h fluid_synth_program_change"
    c'fluid_synth_program_change :: Ptr C'fluid_synth_t -> CInt -> CInt -> IO CInt
