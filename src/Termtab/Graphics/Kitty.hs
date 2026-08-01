{- | Kitty graphics protocol encoder.

Serializes an RGBA image into the APC escape sequences the Kitty graphics
protocol uses (supported by Kitty, WezTerm, and Ghostty). Raw 32-bit RGBA is
transmitted (@f=32@) and displayed at the cursor (@a=T@); the base64 payload is
split into <=4096-byte chunks framed with @m=1@/@m=0@ per the protocol.

This module is pure: it produces the exact bytes to write to the terminal. It
performs no IO and does not know about vty — the caller decides where the
cursor is and when to emit.
-}
module Termtab.Graphics.Kitty (
    encodeImage,
    encodeRGBA,
) where

import Codec.Picture (Image (..), PixelRGBA8, imageData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Internal qualified as BSI
import Data.Vector.Storable qualified as VS
import Data.Word (Word8)
import Foreign.ForeignPtr (castForeignPtr)

-- | Encode a JuicyPixels RGBA image as a Kitty graphics escape sequence.
encodeImage :: Image PixelRGBA8 -> ByteString
encodeImage img =
    encodeRGBA (imageWidth img) (imageHeight img) (vectorToBS (imageData img))

{- | Encode raw row-major RGBA bytes (@w*h*4@ bytes) as a Kitty graphics escape
sequence.
-}
encodeRGBA :: Int -> Int -> ByteString -> ByteString
encodeRGBA w h rgba =
    case chunksOf maxChunk (B64.encode rgba) of
        [] -> BS.empty
        [only] -> frame (baseCtrl <> BC.pack ",m=0") only
        (c0 : rest) -> BS.concat (frame (baseCtrl <> BC.pack ",m=1") c0 : go rest)
  where
    -- Kitty caps each transmission chunk at 4096 bytes of base64 payload.
    maxChunk = 4096
    baseCtrl = BC.pack ("a=T,f=32,s=" <> show w <> ",v=" <> show h)
    go [] = []
    go [lastC] = [frame (BC.pack "m=0") lastC]
    go (c : cs) = frame (BC.pack "m=1") c : go cs

-- | Wrap control data + payload in an @ESC _G <ctrl> ; <payload> ESC \\@ frame.
frame :: ByteString -> ByteString -> ByteString
frame ctrl payload =
    BS.concat [apcStart, ctrl, BC.singleton ';', payload, apcEnd]

apcStart :: ByteString
apcStart = BS.pack [0x1b] <> BC.pack "_G"

apcEnd :: ByteString
apcEnd = BS.pack [0x1b] <> BC.pack "\\"

chunksOf :: Int -> ByteString -> [ByteString]
chunksOf n bs
    | BS.null bs = []
    | otherwise = let (h, t) = BS.splitAt n bs in h : chunksOf n t

-- | Zero-copy view of a storable 'Word8' vector as a 'ByteString'.
vectorToBS :: VS.Vector Word8 -> ByteString
vectorToBS v =
    let (fptr, len) = VS.unsafeToForeignPtr0 v
     in BSI.fromForeignPtr (castForeignPtr fptr) 0 len
