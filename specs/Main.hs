module Main (main) where

import qualified Specs.AppStateUpdateSpec
import qualified Specs.KeySequenceSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.KeySequenceSpec.test
  Specs.AppStateUpdateSpec.test
