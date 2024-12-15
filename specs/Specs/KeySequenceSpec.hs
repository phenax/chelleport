module Specs.KeySequenceSpec where

import Chelleport.KeySequence (findMatchPosition, generateGrid, nextChars)
import qualified Debug.Trace as Debug
import Test.Hspec

test = do
  describe "#nextChars" $ do
    context "when there is a partial match" $ do
      it "filters key sequence and returns next characters" $ do
        nextChars "AB" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Just "CD"
        nextChars "A" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Just "BM"

    context "when there is an exact match" $ do
      it "returns next characters" $ do
        nextChars "ABD" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Just ""

    context "when there are no matches" $ do
      it "returns nothing" $ do
        nextChars "FOO" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Nothing

  describe "#generateGrid" $ do
    it "generates grid of key sequences" $ do
      generateGrid 0 (4, 4) "ABCDEF"
        `shouldBe` Just [["AE", "BD", "CC", "BB"], ["AC", "FD", "EE", "EC"], ["FB", "EA", "DF", "DD"], ["CA", "DB", "CE", "BF"]]

    context "when the the keys set is too short" $ do
      it "cycles back to first character" $ do
        generateGrid 0 (4, 4) "AB"
          `shouldBe` Nothing

  describe "#findMatchPosition" $ do
    it "returns the position of the matching key sequence" $ do
      findMatchPosition "ABD" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
        `shouldBe` Just (1, 2)

    context "when sequence is incomplete" $ do
      it "returns nothing" $ do
        findMatchPosition "AB" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Nothing

    context "when there are no matches" $ do
      it "returns nothing" $ do
        findMatchPosition "FOO" [["XYZ", "ABC"], ["AMK", "BBL", "ABD"]]
          `shouldBe` Nothing
