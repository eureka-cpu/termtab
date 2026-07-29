module Termtab.Parser.Common (parseFile) where

import Data.Text qualified as T
import System.FilePath (takeExtension)

import Termtab.Parser.GP5 qualified as GP5
import Termtab.Parser.MIDI qualified as MIDI
import Termtab.Types (
    ParseError (..),
    Song,
    UnsupportedFormat (..),
 )

parseFile :: FilePath -> IO (Either ParseError Song)
parseFile path = case takeExtension path of
    ".mid" -> MIDI.parseMidi path
    ".midi" -> MIDI.parseMidi path
    ".gp5" -> GP5.parseGP5 path
    ".gp3" -> unsupported "GP3 not yet supported"
    ".gp4" -> unsupported "GP4 not yet supported"
    ".gpx" -> unsupported "GP6/GPX not yet supported"
    ".gp" -> unsupported "GP7 not yet supported"
    ext -> unsupported ("Unknown file extension: " <> ext)
  where
    unsupported msg =
        return
            ( Left
                ( ParseErrorUnsupported
                    UnsupportedFormat
                        { unsupportedPath = path
                        , unsupportedReason = T.pack msg
                        }
                )
            )
