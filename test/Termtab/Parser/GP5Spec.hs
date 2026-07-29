module Termtab.Parser.GP5Spec (tests) where

import Data.Binary.Get
import Data.ByteString.Lazy qualified as LBS
import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Parser.GP5.Internal

tests :: TestTree
tests =
    testGroup
        "GP5 Parser (binary helpers)"
        [ testCase "getInt8' sign-extends 0xFE to -2 (whole note duration)" $ do
            let bs = LBS.pack [0xFE]
            runGet getInt8' bs @?= (-2)
        , testCase "getInt8' preserves positive: 0x03 -> 3" $ do
            let bs = LBS.pack [0x03]
            runGet getInt8' bs @?= 3
        , testCase "getByteSizeString reads length-prefixed padded field" $ do
            -- 5-byte padded field: length=3, data="abc", padded with nulls
            let bs = LBS.pack [0x03, 0x61, 0x62, 0x63, 0x00, 0x00]
            runGet (getByteSizeString 5) bs @?= "abc"
        , testCase "getIntByteSizeString parses known sequence" $ do
            -- total = 4, strLen = 3, data = "xyz"
            let bs = LBS.pack [0x04, 0x00, 0x00, 0x00, 0x03, 0x78, 0x79, 0x7A]
            runGet getIntByteSizeString bs @?= "xyz"
        , testCase "getVersion parses GP5.10 version string" $ do
            let vStr = "FICHIER GUITAR PRO v5.10"
                padded = take 30 (vStr <> repeat '\0')
                bs = LBS.pack (fromIntegral (length vStr) : map (fromIntegral . fromEnum) padded)
            runGet getVersion bs @?= (5, 10)
        , testCase "getVersion rejects non-GP version string" $ do
            let vStr = "NOT A GUITAR PRO FILE"
                padded = take 30 (vStr <> repeat '\0')
                bs = LBS.pack (fromIntegral (length vStr) : map (fromIntegral . fromEnum) padded)
            case runGetOrFail getVersion bs of
                Left _ -> return ()
                Right (_, _, _) -> assertFailure "expected parse failure for non-GP5 version"
        ]
