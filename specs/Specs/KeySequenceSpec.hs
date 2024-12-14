module Specs.KeySequenceSpec where

import Chelleport.KeySequence (nextChars)
import Test.Hspec

test = do
  describe "#nextChars" $ do
    it "filters key sequence and returns next characters" $ do
      nextChars "AB" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
        `shouldBe` Just "CD"
      nextChars "A" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
        `shouldBe` Just "BM"

    context "when exact match is present" $ do
      it "returns next characters" $ do
        nextChars "ABD" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Just ""

    context "when there are no matches" $ do
      it "returns nothing" $ do
        nextChars "FOO" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Nothing
