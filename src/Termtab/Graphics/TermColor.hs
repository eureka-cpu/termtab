{- | Query the terminal's foreground color so rasterized notation can be drawn
in the user's theme color instead of a hardcoded one.

A terminal graphics image is raw pixels and cannot inherit terminal color
attributes the way text cells do. To make the notation "use the terminal's text
color" we ask the terminal for its foreground RGB via the @OSC 10@ query
(@ESC ] 10 ; ? BEL@) and paint the ink with the reply. If the terminal does not
answer (or stdin/stdout is not a TTY), we fall back to a neutral light ink.
-}
module Termtab.Graphics.TermColor (
    queryForegroundColor,
    foregroundOrDefault,
    defaultInk,
    parseOSCColor,
) where

import Codec.Picture (PixelRGBA8 (..))
import Control.Exception (SomeException, bracket, try)
import Data.Char (digitToInt, isHexDigit)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import System.IO
import System.Timeout (timeout)

{- | Ink used when the terminal foreground can't be determined: opaque light
gray, legible on a dark background.
-}
defaultInk :: PixelRGBA8
defaultInk = PixelRGBA8 0xD0 0xD0 0xD0 0xFF

-- | The terminal foreground as opaque ink, or 'defaultInk' if unavailable.
foregroundOrDefault :: IO PixelRGBA8
foregroundOrDefault = do
    m <- queryForegroundColor
    pure $ case m of
        Just (r, g, b) -> PixelRGBA8 r g b 0xFF
        Nothing -> defaultInk

{- | Query the terminal foreground color via OSC 10. Returns 'Nothing' if
stdin/stdout is not a terminal or no reply arrives in time.
-}
queryForegroundColor :: IO (Maybe (Word8, Word8, Word8))
queryForegroundColor = do
    inTty <- hIsTerminalDevice stdin
    outTty <- hIsTerminalDevice stdout
    if not (inTty && outTty)
        then pure Nothing
        else do
            result <-
                try (withRawStdin queryOnce) ::
                    IO (Either SomeException (Maybe (Word8, Word8, Word8)))
            pure (either (const Nothing) id result)

-- | Temporarily disable echo and line buffering on stdin, restoring after.
withRawStdin :: IO a -> IO a
withRawStdin act = bracket save restore (const act)
  where
    save = do
        echo <- hGetEcho stdin
        buf <- hGetBuffering stdin
        hSetEcho stdin False
        hSetBuffering stdin NoBuffering
        pure (echo, buf)
    restore (echo, buf) = do
        hSetEcho stdin echo
        hSetBuffering stdin buf

queryOnce :: IO (Maybe (Word8, Word8, Word8))
queryOnce = do
    hPutStr stdout "\ESC]10;?\a"
    hFlush stdout
    parseOSCColor <$> readResponse

-- | Read the OSC reply until BEL, a short per-char timeout, or a length cap.
readResponse :: IO String
readResponse = go [] (0 :: Int)
  where
    go acc n
        | n >= 64 = pure (reverse acc)
        | otherwise = do
            mc <- timeout 150000 (hGetChar stdin)
            case mc of
                Nothing -> pure (reverse acc)
                Just '\a' -> pure (reverse acc)
                Just c -> go (c : acc) (n + 1)

{- | Parse an OSC 10 color reply, e.g. @ESC]10;rgb:eaea/ebeb/f0f0 BEL@, into an
8-bit RGB triple. Each hex group may be 1-4 digits and is scaled to 0-255.
-}
parseOSCColor :: String -> Maybe (Word8, Word8, Word8)
parseOSCColor s = do
    body <- afterRgb s
    let (g1, r1) = span isHexDigit body
    r1' <- stripSlash r1
    let (g2, r2) = span isHexDigit r1'
    r2' <- stripSlash r2
    let g3 = takeWhile isHexDigit r2'
    if not (null g1) && not (null g2) && not (null g3)
        then Just (scaleHex g1, scaleHex g2, scaleHex g3)
        else Nothing
  where
    stripSlash ('/' : t) = Just t
    stripSlash _ = Nothing

afterRgb :: String -> Maybe String
afterRgb xs
    | "rgb:" `isPrefixOf` xs = Just (drop 4 xs)
afterRgb (_ : rest) = afterRgb rest
afterRgb [] = Nothing

-- | Parse a hex group and scale it from its own bit width down to 8 bits.
scaleHex :: String -> Word8
scaleHex hex =
    let v = foldl (\a c -> a * 16 + digitToInt c) 0 hex
        maxv = 16 ^ length hex - 1
     in fromIntegral (v * 255 `div` maxv)
