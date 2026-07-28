# termtab — Specification

TUI guitar tablature and score editor with audio playback, replicating TuxGuitar's core functionality in Haskell.

---

## Goals

- Full-featured terminal-based tab/score editor for guitar, bass, and standard instruments
- Real-time audio playback with a self-contained audio engine (no external synth required)
- First-class Nix packaging targeting nixpkgs
- MIDI and Guitar Pro 5 as the primary supported formats, with stubs for other formats

---

## Build System

### Language & Toolchain

- Haskell, built with Cabal
- Nix for packaging and dev environment

### Nix Integration

- Use `haskellPackages.callCabal2nix` as the primary approach
- Fall back to `haskell.nix` only if `callCabal2nix` proves insufficient for C dependency management
- The package build replaces the `# TODO: Add package build` comment in `release.nix`
- The package check replaces the `# TODO: Add package checks` comment in `checks`

### Formatters (treefmt)

Add to `formattingOptions.programs` in `release.nix`:

- `nixpkgs-fmt` — already present
- `yamlfmt` — already present
- `stylish-haskell` — add for `.hs` files

### Linters (checks)

Add to `checks` in `release.nix`:

- `hlint` check against all Haskell sources

### Distribution

- GitHub source releases
- nixpkgs package submission (primary registry target)

---

## Phase 0: Infrastructure (First Priority)

Before any application code, establish that the Nix build works end-to-end:

1. Create a minimal Cabal project with a "Hello, termtab" executable
2. Wire the Cabal package into `release.nix` via `callCabal2nix`
3. Add `stylish-haskell` to `formattingOptions`
4. Add `hlint` to `checks`
5. Verify `nix build` and `nix develop` both succeed

All subsequent phases build on this foundation.

---

## File Format Support

### Full Support

| Format | Import | Export |
|--------|--------|--------|
| MIDI (`.mid`) | Yes | Yes |
| Guitar Pro 5 (`.gp5`) | Yes | No (initially) |

### Stub Support (parse header only, return structured error)

- GP3 (`.gp3`)
- GP4 (`.gp4`)
- GP6 (`.gpx` XML-based)
- GP7 (`.gp` JSON-based)

Stubs should expose a `parseFile :: FilePath -> IO (Either UnsupportedFormat Song)` interface so they can be filled in later without changing call sites.

---

## Audio Engine

### Backend Architecture

The audio engine is abstracted behind a typeclass (or sum type) so backends are swappable at runtime:

```
data AudioBackend = FluidSynth | PortMidi | ...
```

The user selects the backend via config or CLI flag. The default is FluidSynth.

### FluidSynth Backend (default)

- Bindings: `bindings-fluidsynth`
- SoundFont: bundled with the package as a Nix resource (e.g. `FluidR3_GM.sf2`); user can override the path in config
- Initialization: create `Synth` and `AudioDriver` on startup, load SoundFont
- Note scheduling: `fluid_synth_noteon` / `fluid_synth_noteoff` with `threadDelay`

### PortMidi Backend (future)

- Outputs to an external MIDI device or software synth
- No bundled SoundFont needed

### Concurrency Model

- The audio engine runs in its own thread
- A `TQueue AudioCommand` receives commands from the UI thread
- Commands: `Play`, `Stop`, `Pause`, `Seek Beat`, `NoteOn Note`, `NoteOff Note`
- The UI thread never blocks on audio

---

## Data Model

### Core Types

```haskell
data Song = Song
  { songTitle    :: Text
  , songArtist   :: Text
  , songTempo    :: Tempo        -- initial BPM
  , songTracks   :: [Track]
  , songMeasures :: [Measure]    -- shared measure grid
  }

data Track = Track
  { trackName       :: Text
  , trackInstrument :: Instrument
  , trackChannel    :: MidiChannel
  , trackBeats      :: Map MeasureIndex [Beat]
  }

data Instrument
  = Guitar { tuning :: [Pitch], stringCount :: Int }
  | Bass   { tuning :: [Pitch], stringCount :: Int }
  | Standard MidiProgram

data Measure = Measure
  { measureIndex    :: MeasureIndex
  , timeSignature   :: TimeSignature
  , keySignature    :: KeySignature
  , tempoChange     :: Maybe Tempo
  }

data Beat = Beat
  { beatDuration :: Duration
  , beatNotes    :: [Note]
  , beatIsRest   :: Bool
  }

data Note = Note
  { notePitch    :: Pitch
  , noteVelocity :: Velocity
  , noteEffects  :: [NoteEffect]
  -- Guitar-specific (Nothing for standard instruments)
  , noteString   :: Maybe StringIndex
  , noteFret     :: Maybe FretNumber
  }

data NoteEffect
  = Bend BendValue
  | Slide SlideType
  | HammerOn
  | PullOff
  | Vibrato
  | PalmMute
```

---

## User Interface

### Framework

- `brick` for the TUI layout and event loop
- `vty` as the terminal backend

### Display Modes

Per-track toggle, persisted in `AppState`:

| Track Type | Available Modes |
|------------|-----------------|
| Guitar / Bass | Tab only, Standard notation only, Both |
| Other instruments | Standard notation only |

Toggle keybinding: `t` cycles through available modes for the focused track.

### Layout

```
┌─ Track 1: Guitar ──────────────────────────────────────────┐
│  [tab / notation / both based on mode]                     │
├─ Track 2: Bass ────────────────────────────────────────────┤
│  [tab / notation / both]                                   │
├─ Status bar ───────────────────────────────────────────────┤
│  File: song.gp5   Tempo: 120 BPM   4/4   Bar: 3/16  [|||]  │
└────────────────────────────────────────────────────────────┘
```

### Tablature Widget

A custom `brick` `Widget` that:

- Uses `getContext` to determine terminal width
- Draws N horizontal lines (6 for guitar, 4 for bass)
- Renders fret numbers at column positions derived from beat offsets and zoom level
- Highlights the playhead cursor column during playback
- Highlights the edit cursor during editing

### App State

```haskell
data AppState = AppState
  { song           :: Song
  , currentMeasure :: MeasureIndex
  , currentBeat    :: BeatIndex
  , currentTrack   :: TrackIndex
  , currentString  :: StringIndex       -- guitar/bass only
  , playbackStatus :: PlaybackStatus    -- Playing | Paused | Stopped
  , displayModes   :: Map TrackIndex DisplayMode
  , audioBackend   :: AudioBackend
  , zoom           :: Int               -- columns per beat
  }
```

### Keybindings

Keybindings should feel like a modal editor, we should make this as idiomatic as possible.
I prefer Helix, so let's go with Helix driven modal options as much as possible. This should
be part of a configuration file we deserialize from `~/.config/termtab`, so we should write
this in such a way that we could eventually expand this to include custom key bindings that
users may want to set, or we later merge upstream.

`esc` is back to navigate (normal mode), `v` is visual (select mode), insert mode is automatic
based on when a user hits a key which inserts something, like a rest or fret number.
`:` is for giving extra commands.

`Space` opens a context menu, for instance to change time signature at the current cursor
position, `Space` to open the content menu, `t` for time operations, `s` for signature.

To go to a measure, `g` to go to a location, the measure number eg. `12`, then `g` again to go there.
`g<MARKER>g`, same as go to a measure except go to a marker, takes a string value (while typing the tui should show where the marker is, but not execute, allow the user to temporarily jump in and out of contexts) |

We need to be able to expand this, so this is non-exhaustive but here's a good starting point:

| Key | Action |
|-----|--------|
| `:p`/`:play` | Play |
| `:l`/`:loop` | Loop, optionally takes a starting measure, otherwise start at the beginning, optionally takes an ending measure, otherwise loop to end |
| `i` | Insert to add string text above the staff for context notes, like "Use 5th position", Esc to leave |
| `:m`/`:mark` | Add a marker |
| `Esc` | Pause/Stop/Leave visual mode/Exit menu |
| `h` `l` | Move cursor left/right (beat) |
| `k` `j` | Move cursor up/down (string, for guitar/bass) |
| `b` `w` | Move by note grouping (for instance, consider 4 16th notes as a word) |
| `g` | Go to, so `gg`/`ge` (go to start/end of track) and `gh`/`gl` (to go start/end of measure) |
| `Shft+b` `Shft+w` | Move by measure |
| `0`–`9` | Enter fret number on current string/beat |
| `d`/`Backspace` | Delete note or selection at cursor |
| `r` | Insert rest at cursor or selection (all selected notes become rests) |
| `u`/`U` | Under/Redo |
| `Shft+Tab` | Cycle note value |
| `t` | Cycle display mode for focused track |
| `Ctrl++` `Ctrl+-` | Zoom in/out |
| `Ctrl+s`/`:w`/`:write`/`:save` | Save file |
| `Ctrl+o`/`:o`/`:open` | Open file |
| `:q`/`:quit`/`:exit` | Quit |

Duration entry, effect application, time/key signature changes, and tempo changes are follow-on editing features within Phase 5+.

---

## Synchronization (Playback ↔ UI)

- A background thread ("tick thread") is spawned when playback starts
- It reads the tempo map and beat sequence from the immutable `Song`
- For each beat: sends `NoteOn`/`NoteOff` to the audio `TQueue`, then updates a `TVar BeatPosition`
- `brick`'s event loop is woken by writing to a `BChan AppEvent` when the `TVar` changes
- The UI redraws the playhead to the new position on each event

---

## Roadmap

### Phase 0 — Nix Infrastructure
- Hello world Haskell executable in `release.nix`
- `stylish-haskell` formatter + `hlint` check wired up

### Phase 1 — Data Model + MIDI
- Define all core types
- MIDI import via `HCodecs`; MIDI export
- Print parsed note data to console (no UI)

### Phase 2 — GP5 Parsing
- GP5 binary format parser
- Stubs for GP3/GP4/GP6/GP7

### Phase 3 — Audio Engine
- FluidSynth backend behind `AudioBackend` abstraction
- Play a single note, then a sequence, from the console

### Phase 4 — Static UI
- `brick` layout with tab/notation widgets
- Displays parsed song statically, no playback

### Phase 5 — Playback Integration
- Tick thread + `TVar` + `BChan` synchronization
- Play/Pause/Stop; playhead cursor follows music

### Phase 6 — Editing
- Note entry, deletion, rest insertion
- Duration entry
- Effect entry (bend, slide, hammer-on, pull-off, vibrato, palm mute)
- Time/key signature and tempo changes
- Multiple track editing

### Phase 7 — Polish & Distribution
- PortMidi backend
- GP5 export
- Config file (backend selection, SoundFont path, keybinding overrides)
- nixpkgs PR preparation

---

## Out of Scope (v1)

- Lyrics
- Chord diagrams
- GP3/GP4/GP6/GP7 full parsing (stubs only)
- GP6/GP7 export
- MIDI export from GP files (convert GP → internal → MIDI is a stretch goal)
