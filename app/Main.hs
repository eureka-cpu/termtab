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
import Termtab.Display (printSong)
import Termtab.Parser.Common (parseFile)
import Termtab.Types

data Action
    = ActionDisplay
    | ActionPlayNote Int
    | ActionPlaySong

data Options = Options
    { optFile :: Maybe FilePath
    , optAction :: Action
    }

actionParser :: Parser Action
actionParser = playNoteP <|> playSongP <|> pure ActionDisplay
  where
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
    song <- case optFile opts of
        Nothing -> return defaultSong
        Just path -> do
            result <- parseFile path
            case result of
                Left err -> hPrint stderr err >> exitFailure
                Right s -> return s
    case optAction opts of
        ActionDisplay ->
            -- TODO Phase 4: replace printSong with the brick TUI
            printSong song
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
