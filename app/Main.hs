module Main (main) where

import Data.Map.Strict qualified as Map
import Data.Version (showVersion)
import Options.Applicative
import Paths_termtab (version)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPrint, hPutStrLn, stderr)

import Termtab.Audio (AudioConfig (..), BackendType (..), withAudioEngine)
import Termtab.Audio.Playback (playNote, playSequence)
import Termtab.Defaults (defaultSong)
import Termtab.Graphics.Demo (runNotationDemo, runPlaceholderDemo)
import Termtab.Graphics.Health (runHealthCheck)
import Termtab.Parser.Common (parseFile)
import Termtab.Types
import Termtab.UI (runUI)

data Action
    = ActionDisplay
    | ActionPlayNote Int
    | ActionPlaySong
    | ActionHealth
    | ActionNotationDemo
    | ActionPlaceholderDemo

data Options = Options
    { optFile :: Maybe FilePath
    , optAction :: Action
    }

actionParser :: Parser Action
actionParser =
    playNoteP <|> playSongP <|> healthP <|> notationDemoP <|> placeholderDemoP <|> pure ActionDisplay
  where
    healthP =
        flag'
            ActionHealth
            ( long "health"
                <> help "Report the detected terminal graphics protocol and Bravura font status, then exit"
            )
    notationDemoP =
        flag'
            ActionNotationDemo
            ( long "notation-demo"
                <> help "Render the default measure as a Kitty graphics image at the cursor, then exit"
            )
    placeholderDemoP =
        flag'
            ActionPlaceholderDemo
            ( long "placeholder-demo"
                <> help "Render the default measure via Kitty Unicode placeholder cells, then exit"
            )
    playNoteP =
        ActionPlayNote
            <$> option
                auto
                ( long "play-note"
                    <> metavar "MIDI_NOTE"
                    <> help "Play a single MIDI note (0-127, e.g. 60 for middle C)"
                )
    playSongP =
        flag'
            ActionPlaySong
            ( long "play"
                <> help "Play the loaded song (or default scratch track)"
            )

fileParser :: Parser (Maybe FilePath)
fileParser =
    optional $
        argument
            str
            ( metavar "FILE"
                <> help "Guitar Pro or MIDI file to open (omit to start with a blank scratch track)"
            )

optionsParser :: Parser Options
optionsParser = Options <$> fileParser <*> actionParser

versionOption :: Parser (a -> a)
versionOption =
    infoOption
        (showVersion version)
        ( long "version"
            <> short 'V'
            <> help "Print version information"
        )

parserInfo :: ParserInfo Options
parserInfo =
    info
        (optionsParser <**> versionOption <**> helper)
        ( fullDesc
            <> progDesc "TUI guitar tablature and score editor"
            <> header "termtab - a terminal guitar tab editor"
        )

getSoundFontPath :: IO FilePath
getSoundFontPath = do
    mPath <- lookupEnv "TERMTAB_SOUNDFONT"
    case mPath of
        Just path -> return path
        Nothing -> do
            hPutStrLn stderr "Error: No SoundFont found."
            hPutStrLn stderr "Set TERMTAB_SOUNDFONT=/path/to/file.sf2 or use 'nix develop' to get one automatically."
            exitFailure

main :: IO ()
main = do
    opts <- customExecParser (prefs showHelpOnEmpty) parserInfo
    case optAction opts of
        ActionHealth -> runHealthCheck
        ActionNotationDemo -> runNotationDemo
        ActionPlaceholderDemo -> runPlaceholderDemo
        _ -> runWithSong opts

-- | Load the requested song (or the default scratch track) and dispatch.
runWithSong :: Options -> IO ()
runWithSong opts = do
    song <- case optFile opts of
        Nothing -> return defaultSong
        Just path -> do
            result <- parseFile path
            case result of
                Left err -> hPrint stderr err >> exitFailure
                Right s -> return s
    case optAction opts of
        ActionHealth -> return () -- handled in main
        ActionNotationDemo -> return () -- handled in main
        ActionPlaceholderDemo -> return () -- handled in main
        ActionDisplay ->
            runUI (optFile opts) song
        ActionPlayNote midiNote -> do
            sfPath <- getSoundFontPath
            let cfg = AudioConfig{acSoundFontPath = sfPath}
            withAudioEngine BackendFluidSynth cfg $ \engine ->
                playNote engine (MidiChannel 0) (Pitch midiNote) (Velocity 100) Quarter (Tempo 120)
        ActionPlaySong -> do
            sfPath <- getSoundFontPath
            let cfg = AudioConfig{acSoundFontPath = sfPath}
            withAudioEngine BackendFluidSynth cfg $ \engine ->
                case songTracks song of
                    [] -> hPutStrLn stderr "No tracks to play."
                    (track : _) -> do
                        let beats = concatMap snd $ Map.toAscList (trackBeats track)
                        playSequence engine (trackChannel track) (songTempo song) beats
