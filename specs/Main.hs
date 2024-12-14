module Main (main) where

import qualified Specs.KeySequenceSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.KeySequenceSpec.test
