module Specs.KeySequenceSpec where

import Chelleport.KeySequence (generateKeyCells, nextChars)
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

  describe "#generateKeyCells" $ do
    it "generates grid of key sequences" $ do
      generateKeyCells (4, 4) "ABCDEF"
        `shouldBe` [ ["HKA", "HKB", "LKA", "LKB"],
                     ["HKC", "HKD", "LKC", "LKD"],
                     ["HJA", "HJB", "LJA", "LJB"],
                     ["HJC", "HJD", "LJC", "LJD"]
                   ]
    context "when the the keys set is too short" $ do
      it "cycles back to first character" $ do
        generateKeyCells (4, 4) "AB"
          `shouldBe` [ ["HKA", "HKB", "LKA", "LKB"],
                       ["HKA", "HKB", "LKA", "LKB"],
                       ["HJA", "HJB", "LJA", "LJB"],
                       ["HJA", "HJB", "LJA", "LJB"]
                     ]
