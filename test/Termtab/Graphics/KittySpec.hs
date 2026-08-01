module Termtab.Graphics.KittySpec (tests) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Graphics.Kitty

-- ESC _G ... ; ... ESC \
esc :: ByteString
esc = BS.pack [0x1b]

apcStart :: ByteString
apcStart = esc <> BC.pack "_G"

apcEnd :: ByteString
apcEnd = esc <> BC.pack "\\"

-- Count non-overlapping occurrences of a needle.
countOcc :: ByteString -> ByteString -> Int
countOcc needle = go 0
  where
    go !n hay = case BS.breakSubstring needle hay of
        (_, rest)
            | BS.null rest -> n
            | otherwise -> go (n + 1) (BS.drop (BS.length needle) rest)

infixOf :: String -> ByteString -> Bool
infixOf s = BS.isInfixOf (BC.pack s)

tests :: TestTree
tests =
    testGroup
        "Graphics.Kitty"
        [ testGroup
            "single chunk (small image)"
            [ testCase "is one APC frame" $
                countOcc apcStart small @?= 1
            , testCase "starts with ESC _G" $
                BS.isPrefixOf apcStart small @?= True
            , testCase "ends with ESC backslash" $
                BS.isSuffixOf apcEnd small @?= True
            , testCase "carries RGBA control keys and dimensions" $ do
                assertBool "a=T" (infixOf "a=T" small)
                assertBool "f=32" (infixOf "f=32" small)
                assertBool "s=2" (infixOf "s=2" small)
                assertBool "v=2" (infixOf "v=2" small)
            , testCase "final chunk marked m=0" $
                assertBool "m=0" (infixOf "m=0" small)
            ]
        , testGroup
            "multi chunk (large image forces >4096 base64 bytes)"
            [ testCase "splits into multiple frames" $
                assertBool "more than one frame" (countOcc apcStart large > 1)
            , testCase "control keys only on the first frame" $
                countOcc (BC.pack "f=32") large @?= 1
            , testCase "non-final frames marked m=1" $
                assertBool "has m=1" (infixOf "m=1" large)
            , testCase "last frame marked m=0" $
                assertBool "has m=0" (infixOf "m=0" large)
            ]
        , testCase "empty image yields no output" $
            encodeRGBA 0 0 BS.empty @?= BS.empty
        ]
  where
    -- 2x2 RGBA = 16 bytes -> base64 fits one chunk.
    small = encodeRGBA 2 2 (BS.replicate 16 0)
    -- 40x40 RGBA = 6400 bytes -> ~8536 base64 bytes -> 3 chunks.
    large = encodeRGBA 40 40 (BS.replicate (40 * 40 * 4) 200)
