module Specs.ViewSpec where

import Chelleport.Config
import Chelleport.Types
import Chelleport.View
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  let defaultState = defaultAppState {stateGrid = [["ABC", "DEF"], ["DJK", "JKL"]]}
  let drawTextCalls = filter (\case Mock_drawText {} -> True; _ -> False) . calls

  describe "#render" $ do
    context "when key sequence is empty" $ do
      let currentState = defaultState {stateKeySequence = ""}

      it "draws matching text labels" $ do
        (_, mock) <- runWithMocks $ render currentState
        drawTextCalls mock
          `shouldBe` [ Mock_drawText (460, 10) colorWhite "ABC",
                       Mock_drawText (1420, 10) colorWhite "DEF",
                       Mock_drawText (460, 550) colorWhite "DJK",
                       Mock_drawText (1420, 550) colorWhite "JKL"
                     ]

    context "when there is a partial match" $ do
      let currentState = defaultState {stateKeySequence = "D"}

      it "draws matching text labels" $ do
        (_, mock) <- runWithMocks $ render currentState
        drawTextCalls mock
          `shouldBe` [ Mock_drawText (1420, 10) colorLightGray "D",
                       Mock_drawText (1430, 10) colorAccent "EF",
                       Mock_drawText (460, 550) colorLightGray "D",
                       Mock_drawText (470, 550) colorAccent "JK"
                     ]

    context "when key sequence is complete match" $ do
      let currentState = defaultState {stateKeySequence = "DEF"}

      it "draws only the matching label" $ do
        (_, mock) <- runWithMocks $ render currentState
        drawTextCalls mock `shouldBe` [Mock_drawText (1420, 10) colorLightGray "DEF"]

  describe "#renderKeySequence" $ do
    context "when there is a partial match" $ do
      it "draws the matched section and highlights the remaining characters" $ do
        (_, mock) <- runWithMocks $ renderKeySequence "ABC" "ABCDE" (0, 0)
        calls mock
          `shouldBe` [Mock_drawText (0, 0) colorLightGray "ABC", Mock_drawText (3 * 10, 0) colorAccent "DE"]

      it "return true as the text is visible" $ do
        (isVisible, _) <- runWithMocks $ renderKeySequence "ABC" "ABCDE" (0, 0)
        isVisible `shouldBe` True

    context "when there is no input key sequence" $ do
      it "draws text as a single chunk" $ do
        (_, mock) <- runWithMocks $ renderKeySequence "" "ABCD" (0, 0)
        calls mock `shouldBe` [Mock_drawText (0, 0) colorWhite "ABCD"]

      it "return true as the text is visible" $ do
        (isVisible, _) <- runWithMocks $ renderKeySequence "" "ABCD" (0, 0)
        isVisible `shouldBe` True

    context "when key sequence does not match" $ do
      it "does not draw text" $ do
        (_, mock) <- runWithMocks $ renderKeySequence "AXY" "ABCD" (0, 0)
        calls mock `shouldBe` []

      it "return false as the text is not visible" $ do
        (isVisible, _) <- runWithMocks $ renderKeySequence "AXY" "ABCD" (0, 0)
        isVisible `shouldBe` False
