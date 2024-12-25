module Main (main) where

import qualified Specs.AppEventSpec
import qualified Specs.AppStateSpec
import qualified Specs.KeySequenceSpec
import qualified Specs.ViewSpec
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Specs.AppEventSpec.test
  Specs.AppStateSpec.test
  Specs.KeySequenceSpec.test
  Specs.ViewSpec.test
