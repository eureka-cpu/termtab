{- | The @--health@ report.

Prints what termtab has decided about notation rendering: which terminal
graphics protocol it will use (and why), the environment signals it read, and
whether the bundled Bravura font actually loads and rasterizes. Lets you verify
detection without launching the TUI.
-}
module Termtab.Graphics.Health (runHealthCheck) where

import Codec.Picture (imageHeight, imageWidth)
import Control.Exception (SomeException, try)
import Data.Word (Word8)
import Numeric (showHex)
import System.Environment (lookupEnv)

import Termtab.Graphics.Detect
import Termtab.Graphics.FreeType
import Termtab.Graphics.TermColor (queryForegroundColor)

-- | Result of trying to load and rasterize a glyph from the Bravura font.
data FontStatus
    = FontUnset
    | FontError FilePath String
    | FontOk FilePath Int Int Int -- path, width, height, advance (px)

runHealthCheck :: IO ()
runHealthCheck = do
    info <- detectProtocolInfo
    fg <- queryForegroundColor
    font <- checkFont
    mapM_ putStrLn (report info fg font)

report :: DetectionInfo -> Maybe (Word8, Word8, Word8) -> FontStatus -> [String]
report info fg font =
    [ "termtab health check"
    , "===================="
    , ""
    , "Terminal graphics"
    , "  TERM             = " <> orUnset (diTerm info)
    , "  TERM_PROGRAM     = " <> orUnset (diTermProgram info)
    , "  KITTY_WINDOW_ID  = " <> orUnset (diKittyWindowId info)
    , "  TERMTAB_GRAPHICS = " <> orUnset (diOverride info)
    , "  -> protocol: " <> showProtocol (diProtocol info)
    , "     reason:   " <> diReason info
    , "  ink color (OSC 10 foreground): " <> maybe "(no reply — will use default ink)" fmtColor fg
    , ""
    , "Notation font (Bravura)"
    ]
        <> fontLines font
        <> [ ""
           , "Notation rendering: " <> summary (diProtocol info) font
           ]

fontLines :: FontStatus -> [String]
fontLines FontUnset =
    [ "  TERMTAB_BRAVURA_FONT = (unset)"
    , "  status: font path not configured — set TERMTAB_BRAVURA_FONT or use the Nix devshell"
    ]
fontLines (FontError path err) =
    [ "  TERMTAB_BRAVURA_FONT = " <> path
    , "  status: FAILED to load — " <> err
    ]
fontLines (FontOk path w h adv) =
    [ "  TERMTAB_BRAVURA_FONT = " <> path
    , "  status: OK"
    , "  test glyph U+E0A4 (notehead): "
        <> show w
        <> "x"
        <> show h
        <> " px, advance "
        <> show adv
        <> " px"
    ]

-- | One-line bottom-line summary of what will actually happen.
summary :: GraphicsProtocol -> FontStatus -> String
summary TextOnly _ =
    "DISABLED — terminal has no graphics support; notation falls back to the Unicode text staff"
summary proto (FontOk _ _ _ _) =
    "ENABLED — TabAndNotation renders Bravura images via the " <> showProtocol proto
summary proto _ =
    "DEGRADED — "
        <> showProtocol proto
        <> " is available but Bravura did not load; notation falls back to the Unicode text staff"

-- | Load Bravura and rasterize a notehead, capturing any failure.
checkFont :: IO FontStatus
checkFont = do
    mPath <- lookupEnv "TERMTAB_BRAVURA_FONT"
    case mPath of
        Nothing -> pure FontUnset
        Just path -> do
            result <-
                try (rasterizeTestGlyph path) ::
                    IO (Either SomeException (Int, Int, Int))
            pure $ case result of
                Left err -> FontError path (show err)
                Right (w, h, adv) -> FontOk path w h adv

-- | Rasterize the SMuFL black notehead; returns its dimensions and advance.
rasterizeTestGlyph :: FilePath -> IO (Int, Int, Int)
rasterizeTestGlyph path =
    withFontFace path $ \face -> do
        mg <- renderGlyph face 40 '\xE0A4'
        case mg of
            Nothing -> error "FreeType could not render the test glyph"
            Just gb ->
                let img = gbImage gb
                 in pure (imageWidth img, imageHeight img, gbAdvance gb)

showProtocol :: GraphicsProtocol -> String
showProtocol Kitty = "Kitty graphics protocol"
showProtocol Sixel = "Sixel"
showProtocol TextOnly = "Unicode text (no graphics)"

orUnset :: Maybe String -> String
orUnset = maybe "(unset)" id

fmtColor :: (Word8, Word8, Word8) -> String
fmtColor (r, g, b) = '#' : concatMap hex2 [r, g, b]
  where
    hex2 w = let s = showHex w "" in if length s < 2 then '0' : s else s
