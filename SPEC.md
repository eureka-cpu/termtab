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

A custom `brick` `Widget` that uses Unicode box-drawing characters for a grid-cell rendering style.

#### Rendering Style

Uses Unicode box-drawing characters (`─`, `│`, `┌`, `┐`, `└`, `┘`, `├`, `┤`, `┬`, `┴`) for a bordered grid. Each beat is a cell with fret numbers centered inside. A **stem row** above the grid shows note durations and rests using vertical stems and beams, matching the rhythm notation style of Guitar Pro.

**Tab rendering (GP-style):**

The tab uses the same visual style as Guitar Pro: continuous horizontal lines for strings, fret numbers sitting on the lines (replacing the line segment), vertical bar lines for measure boundaries, and "TAB" label on the left. This is text-rendered via brick/vty.

```
 22                │23                           │24
T──────────────────│─────────────────────────────│──────────────────────────
A──────────────────│─────────────────────────────│──────────────────────────
B──────────────────│─────────────────────────────│──────────────────────────
───────────────────│─────────────────────────────│──1────1────1────1────1───
─────0───1───0─────│──0──0──0──0──0──0──0────────│──────────────────────────
──0────────────────│──1──1──1──1──1──1──1────────│──1────1────1────1────1───
```

**Key rendering rules:**
- String lines are continuous `─` characters
- Fret numbers replace line segments where notes occur (e.g., `──5──` or `──12──`)
- Single and double-digit frets both sit on the line: `──5──` vs `──12──`
- Measure bar lines `│` run vertically through all strings
- Measure numbers appear above each bar
- "TAB" replaces string tuning labels on the left (GP convention)
- Empty strings show unbroken lines `────────`

**"Both" mode — standard notation rendered as inline Sixel image above tab (GP-style):**

When display mode is `TabAndNotation`, a full graphical notation image renders above the tab grid as a **Sixel image** (treble clef, note heads, stems, beams, rests, accidentals, key/time signatures) matching Guitar Pro's visual quality. Notation glyphs are drawn from the **Bravura** SMuFL font, which termtab rasterizes itself and blits to the terminal. Rendering therefore never depends on the terminal's configured font, on font fallback for Private Use Area codepoints, or on Bravura being installed system-wide — the previous text-glyph approach (emitting SMuFL PUA codepoints as cell text) cannot work reliably because a TUI cannot instruct the terminal which font to use for a cell.

**Critical: the notation image and tab grid must be column-aligned.** The notation renderer must produce an image whose beat positions match the tab's column positions pixel-for-pixel. Measure bar lines run continuously through both.

#### Notation Rendering Pipeline

termtab owns the full rendering pipeline; there is no LilyPond subprocess and no dependency on system-installed fonts.

1. **Layout** — from the visible measures of `Song`, compute a pixel-space layout: staff line y-positions, clef/key/time-signature positions, and each beat's x-position, derived from the tab's column layout so beats align vertically with the tab below.
2. **Rasterize** — draw the layout into an in-memory RGBA image buffer (`JuicyPixels`). Staff lines, stems, beams, and bar lines are drawn as primitives; noteheads, clefs, rests, accidentals, and dynamics are SMuFL glyphs rasterized from the bundled **Bravura** font via FreeType (`freetype2` bindings), which handles Bravura's OpenType/CFF outlines.
3. **Encode** — convert the image buffer to the active terminal graphics protocol (see below) and emit it inline via escape sequences.
4. Image width is computed from the tab's column layout so beat x-positions match tab columns pixel-for-pixel.

#### Font Bundling

- Bravura is provided at build time via Nix (`openlilylib-fonts.bravura`) and its path baked into the binary, mirroring how `TERMTAB_SOUNDFONT` / `soundfont-fluid` is wired for the SoundFont.
- Runtime override via a `TERMTAB_BRAVURA_FONT` env var for development.
- Only the SMuFL codepoints actually used are rasterized; glyph bitmaps are cached per `(glyph, pixel-size)` so glyphs are not re-rasterized every frame.

#### Terminal Graphics Protocol

No single graphics protocol covers all target terminals: **Kitty does not support Sixel** (it implements only its own protocol), while **iTerm2 does not support the Kitty protocol**. Supporting Kitty, iTerm2, and WezTerm therefore requires **two backends selected by runtime detection**. Both consume the same `JuicyPixels` image buffer produced by the pipeline above; only the final encode + emit differs.

**Backend A: Kitty graphics protocol** — `Termtab.Graphics.Kitty`
- Covers Kitty, WezTerm, Ghostty.
- PNG/RGBA payload chunked and base64-encoded, sent via APC (`_G…`) escape sequences.

**Backend B: Sixel** — `Termtab.Graphics.Sixel`
- Covers iTerm2, WezTerm, foot, xterm (`-ti vt340`), mlterm, contour.
- Encode the RGBA buffer → Sixel bytes (hand-rolled, or the `sixel` Hackage package if it fits).

**Detection & selection**
- Prefer Kitty protocol where available (better integration, no palette limits), else Sixel, else the text fallback.
- Detect via `$TERM`/`$TERM_PROGRAM`/`$KITTY_WINDOW_ID` plus a Device Attributes (DA) query for Sixel support; allow an explicit override env var (`TERMTAB_GRAPHICS=kitty|sixel|text`).

**Fallback: Unicode text** (degraded)
- For terminals with no graphics support, fall back to the current Unicode staff placeholder.
- `NotationOnly` mode shows the text stub; `TabAndNotation` shows tab only with a message.

#### vty / Redraw Integration

- vty owns the screen via a cell-diff model; the Sixel payload is written outside that model. termtab positions the cursor to the notation region, emits the Sixel bytes, and reserves those rows so vty's diff does not clobber the image (re-emitting on redraw when the region is dirtied). This coupling with vty's redraw is the main integration risk and is prototyped first (see plan).

#### Nix Integration

- Add `openlilylib-fonts.bravura` to package runtime inputs and the devshell; expose its path to the build like `soundfont-fluid` / `TERMTAB_SOUNDFONT`.
- Add `JuicyPixels` and `freetype2` (with the `freetype` system lib via `pkgconfig-depends`) to cabal deps; the Kitty backend needs `base64-bytestring`; add `sixel` only if the Hackage encoder is used instead of a hand-rolled one.
- No LilyPond, no Cairo, no system font installation required.

#### Beat Spacing

- Beat positions are duration-proportional: a quarter note gets more horizontal space than an eighth
- Minimum spacing: **3 columns** per beat (ensures double-digit frets like `12` fit)
- Quarter note at zoom 4: 4 columns of line characters around the fret number
- Eighth note at zoom 4: max(3, 2) = 3 columns
- Fret numbers replace line segments: `──5──` (single digit) or `──12──` (double digit)

#### String Order

Strings render high to low (standard tab convention):
- Top line: high E (thinnest string)
- Bottom line: low E (thickest string)

#### Visual Indicators

- **Edit cursor**: reverse video on the fret number (or line segment) at `(currentBeat, currentString)`
- **Playhead**: green background on the entire column at `(playheadBeat)` across all strings
- **Visual selection**: blue background on selected beat range (for combine operations)

#### Behavior

- Uses `getContext` to determine terminal width
- Measure width is fixed by time signature (e.g., 4/4 = 4×zoom columns)
- Beats render proportionally within the fixed measure width
- Empty beat positions (beyond the beat list) render as unbroken line segments
- Viewport starts from measure 0; scrolls forward only when cursor exceeds visible area

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
