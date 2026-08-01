module Termtab.Graphics.TermColorSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Graphics.TermColor (parseOSCColor)

tests :: TestTree
tests =
    testGroup
        "Graphics.TermColor.parseOSCColor"
        [ testCase "parses a 16-bit rgb reply (high byte)" $
            parseOSCColor "\ESC]10;rgb:eaea/ebeb/f0f0\a" @?= Just (0xEA, 0xEB, 0xF0)
        , testCase "parses an 8-bit rgb reply" $
            parseOSCColor "\ESC]10;rgb:ff/80/00\a" @?= Just (0xFF, 0x80, 0x00)
        , testCase "scales a 1-digit group to full range" $
            parseOSCColor "rgb:f/0/f" @?= Just (0xFF, 0x00, 0xFF)
        , testCase "rejects a reply with no rgb payload" $
            parseOSCColor "\ESC]10;?\a" @?= Nothing
        , testCase "rejects a truncated reply" $
            parseOSCColor "rgb:ea/eb" @?= Nothing
        ]
