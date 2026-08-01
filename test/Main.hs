module Main (main) where

import Test.Tasty

import Termtab.Audio.PlaybackSpec qualified as PlaybackSpec
import Termtab.Graphics.DetectSpec qualified as DetectSpec
import Termtab.Graphics.FreeTypeSpec qualified as FreeTypeSpec
import Termtab.Graphics.KittySpec qualified as KittySpec
import Termtab.Parser.GP5Spec qualified as GP5Spec
import Termtab.Parser.MIDISpec qualified as MIDISpec
import Termtab.TypesSpec qualified as TypesSpec
import Termtab.UI.EditingSpec qualified as EditingSpec
import Termtab.UI.TypesSpec qualified as UITypesSpec

main :: IO ()
main =
    defaultMain $
        testGroup
            "termtab"
            [ TypesSpec.tests
            , MIDISpec.tests
            , GP5Spec.tests
            , PlaybackSpec.tests
            , UITypesSpec.tests
            , EditingSpec.tests
            , DetectSpec.tests
            , KittySpec.tests
            , FreeTypeSpec.tests
            ]
