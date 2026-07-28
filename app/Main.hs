module Main (main) where

import           Data.Version          (showVersion)
import           Options.Applicative
import           Paths_termtab         (version)
import           System.Exit           (exitFailure)
import           System.IO             (hPutStrLn, stderr)

import           Termtab.Defaults      (defaultSong)
import           Termtab.Display       (printSong)
import           Termtab.Parser.Common (parseFile)

data Options = Options
  { optFile :: Maybe FilePath
  }

fileParser :: Parser (Maybe FilePath)
fileParser = optional $ argument str
  (  metavar "FILE"
  <> help "Guitar Pro or MIDI file to open (omit to start with a blank scratch track)"
  )

optionsParser :: Parser Options
optionsParser = Options <$> fileParser

versionOption :: Parser (a -> a)
versionOption = infoOption (showVersion version)
  (  long "version"
  <> short 'V'
  <> help "Print version information"
  )

parserInfo :: ParserInfo Options
parserInfo = info (optionsParser <**> versionOption <**> helper)
  (  fullDesc
  <> progDesc "TUI guitar tablature and score editor"
  <> header "termtab - a terminal guitar tab editor"
  )

main :: IO ()
main = do
  opts <- customExecParser (prefs showHelpOnEmpty) parserInfo
  song <- case optFile opts of
    Nothing   -> return defaultSong
    Just path -> do
      result <- parseFile path
      case result of
        Left err   -> hPutStrLn stderr (show err) >> exitFailure
        Right song -> return song
  -- TODO Phase 4: replace printSong with the brick TUI
  printSong song
