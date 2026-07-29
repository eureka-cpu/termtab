module Main (main) where

import           Test.Tasty

import qualified Termtab.Parser.GP5Spec  as GP5Spec
import qualified Termtab.Parser.MIDISpec as MIDISpec
import qualified Termtab.TypesSpec       as TypesSpec

main :: IO ()
main = defaultMain $ testGroup "termtab"
  [ TypesSpec.tests
  , MIDISpec.tests
  , GP5Spec.tests
  ]
