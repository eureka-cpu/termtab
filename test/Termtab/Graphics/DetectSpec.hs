module Termtab.Graphics.DetectSpec (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Graphics.Detect

-- Convenience: (override, TERM, TERM_PROGRAM, KITTY_WINDOW_ID)
classify :: Maybe String -> Maybe String -> Maybe String -> Maybe String -> GraphicsProtocol
classify = classifyProtocol

tests :: TestTree
tests =
    testGroup
        "Graphics.Detect"
        [ testGroup
            "override wins"
            [ testCase "kitty override" $
                classify (Just "kitty") (Just "xterm-256color") Nothing Nothing @?= Kitty
            , testCase "sixel override even inside Kitty" $
                classify (Just "sixel") (Just "xterm-kitty") Nothing (Just "1") @?= Sixel
            , testCase "text override" $
                classify (Just "text") Nothing Nothing (Just "1") @?= TextOnly
            , testCase "override is case-insensitive" $
                classify (Just "KITTY") Nothing Nothing Nothing @?= Kitty
            ]
        , testGroup
            "kitty-protocol terminals"
            [ testCase "KITTY_WINDOW_ID set" $
                classify Nothing (Just "xterm-256color") Nothing (Just "3") @?= Kitty
            , testCase "TERM=xterm-kitty" $
                classify Nothing (Just "xterm-kitty") Nothing Nothing @?= Kitty
            , testCase "WezTerm prefers kitty protocol" $
                classify Nothing (Just "xterm-256color") (Just "WezTerm") Nothing @?= Kitty
            , testCase "Ghostty via TERM" $
                classify Nothing (Just "xterm-ghostty") Nothing Nothing @?= Kitty
            ]
        , testGroup
            "sixel terminals"
            [ testCase "iTerm2" $
                classify Nothing (Just "xterm-256color") (Just "iTerm.app") Nothing @?= Sixel
            , testCase "TERM advertises sixel" $
                classify Nothing (Just "xterm-sixel") Nothing Nothing @?= Sixel
            , testCase "foot" $
                classify Nothing (Just "foot") Nothing Nothing @?= Sixel
            ]
        , testGroup
            "no graphics"
            [ testCase "plain xterm" $
                classify Nothing (Just "xterm-256color") Nothing Nothing @?= TextOnly
            , testCase "empty environment" $
                classify Nothing Nothing Nothing Nothing @?= TextOnly
            ]
        ]
