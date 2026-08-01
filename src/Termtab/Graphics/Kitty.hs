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
    encodeVirtual,
    encodeDirect,
    deleteImage,
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

{- | Encode an image for direct placement at the cursor (@a=T@) — used by the
one-shot demo.
-}
encodeImage :: Image PixelRGBA8 -> ByteString
encodeImage img =
    encodeRGBA (imageWidth img) (imageHeight img) (vectorToBS (imageData img))

{- | Encode raw row-major RGBA bytes (@w*h*4@ bytes) for direct placement at the
cursor.
-}
encodeRGBA :: Int -> Int -> ByteString -> ByteString
encodeRGBA w h rgba = frameChunks (baseCtrl w h) rgba

{- | Encode an image as a *virtual* placement (@U=1@) for display via Unicode
placeholders: transmit it under image id @imgId@ occupying @cols@×@rows@ terminal
cells. The image is not shown until placeholder cells referencing @imgId@ are
drawn (see "Termtab.Graphics.KittyPlacement"). @q=2@ suppresses the terminal's
acknowledgement so it cannot corrupt input.
-}
encodeVirtual :: Int -> Int -> Int -> Image PixelRGBA8 -> ByteString
encodeVirtual imgId cols rows img =
    frameChunks (baseCtrl (imageWidth img) (imageHeight img) <> virtualKeys) rgba
  where
    rgba = vectorToBS (imageData img)
    virtualKeys =
        BC.pack (",U=1,i=" <> show imgId <> ",c=" <> show cols <> ",r=" <> show rows <> ",q=2")

{- | Encode an image for direct placement at the cursor, scaled to @cols@×@rows@
cells, under image id @imgId@ / placement id 1. Re-emitting with the same ids
replaces the placement (no buildup). @C=1@ leaves the cursor where it is; @q=2@
suppresses the acknowledgement. Used to blit the notation over a reserved region
whose screen position the caller has moved the cursor to.
-}
encodeDirect :: Int -> Int -> Int -> Image PixelRGBA8 -> ByteString
encodeDirect imgId cols rows img =
    frameChunks (baseCtrl (imageWidth img) (imageHeight img) <> keys) (vectorToBS (imageData img))
  where
    keys =
        BC.pack
            (",i=" <> show imgId <> ",p=1,c=" <> show cols <> ",r=" <> show rows <> ",C=1,q=2")

baseCtrl :: Int -> Int -> ByteString
baseCtrl w h = BC.pack ("a=T,f=32,s=" <> show w <> ",v=" <> show h)

{- | Chunk the base64 payload into <=4096-byte APC frames with the given control
data on the first frame.
-}
frameChunks :: ByteString -> ByteString -> ByteString
frameChunks ctrl rgba =
    case chunksOf maxChunk (B64.encode rgba) of
        [] -> BS.empty
        [only] -> frame (ctrl <> BC.pack ",m=0") only
        (c0 : rest) -> BS.concat (frame (ctrl <> BC.pack ",m=1") c0 : go rest)
  where
    -- Kitty caps each transmission chunk at 4096 bytes of base64 payload.
    maxChunk = 4096
    go [] = []
    go [lastC] = [frame (BC.pack "m=0") lastC]
    go (c : cs) = frame (BC.pack "m=1") c : go cs

{- | Delete image @imgId@ (data and all its placements). Emitted before a
re-blit so Kitty actually refreshes the pixels instead of assuming an unchanged
placement is still current.
-}
deleteImage :: Int -> ByteString
deleteImage imgId =
    BS.concat [apcStart, BC.pack ("a=d,d=I,i=" <> show imgId <> ",q=2"), apcEnd]

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
