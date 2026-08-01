module Termtab.Graphics.FreeTypeSpec (tests) where

import Codec.Picture.Types (Image (..))
import Data.Vector.Storable qualified as VS
import System.Environment (lookupEnv)
import Test.Tasty
import Test.Tasty.HUnit

import Termtab.Graphics.FreeType

{- | Exercises the FreeType FFI end to end by rasterizing a SMuFL glyph from
Bravura. Skipped (trivially passes) when @TERMTAB_BRAVURA_FONT@ is unset so the
suite still runs outside the Nix devshell.
-}
tests :: TestTree
tests =
    testGroup
        "Graphics.FreeType"
        [ testCase "rasterizes a Bravura notehead with ink and advance" $ do
            mFont <- lookupEnv "TERMTAB_BRAVURA_FONT"
            case mFont of
                Nothing -> pure ()
                Just path -> withFontFace path $ \face -> do
                    -- U+E0A4 = SMuFL black (filled) notehead.
                    mg <- renderGlyph face 64 '\xE0A4'
                    case mg of
                        Nothing -> assertFailure "renderGlyph returned Nothing"
                        Just gb -> do
                            let img = gbImage gb
                            assertBool "positive width" (imageWidth img > 0)
                            assertBool "positive height" (imageHeight img > 0)
                            assertBool "glyph has ink" (VS.any (> 0) (imageData img))
                            assertBool "positive advance" (gbAdvance gb > 0)
        ]
