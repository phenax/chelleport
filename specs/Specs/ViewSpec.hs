module Specs.ViewSpec where

import Chelleport.Config
import Chelleport.Types
import Chelleport.View
import Data.Default (Default (def))
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  let defaultState = def {stateGridRows = 2, stateGridCols = 2, stateMode = ModeHints def {stateGrid = [["ABC", "DEF"], ["DJK", "JKL"]]}}

  describe "#render" $ do
    context "when key sequence is empty" $ do
      let currentState = defaultState {stateMode = ModeHints (modeHintsData $ stateMode defaultState) {stateKeySequence = ""}}

      it "draws matching text labels" $ do
        (_, mock) <- runWithMocks $ do
          Mock_windowSize `mockReturns` mockWindowSize
          render currentState
        mock `shouldHaveCalled` Mock_drawText (460, 10) colorWhite FontLG "ABC"
        mock `shouldHaveCalled` Mock_drawText (1420, 10) colorWhite FontLG "DEF"
        mock `shouldHaveCalled` Mock_drawText (460, 550) colorWhite FontLG "DJK"
        mock `shouldHaveCalled` Mock_drawText (1420, 550) colorWhite FontLG "JKL"

    context "when there is a partial match" $ do
      let currentState = defaultState {stateMode = ModeHints (modeHintsData $ stateMode defaultState) {stateKeySequence = "D"}}

      it "draws matching text labels" $ do
        (_, mock) <- runWithMocks $ do
          Mock_windowSize `mockReturns` mockWindowSize
          Mock_drawText (1420, 10) colorLightGray FontLG "D" `mockReturns` (10, 0)
          Mock_drawText (460, 550) colorLightGray FontLG "D" `mockReturns` (10, 0)
          render currentState
        mock `shouldHaveCalled` Mock_drawText (1420, 10) colorLightGray FontLG "D"
        mock `shouldHaveCalled` Mock_drawText (1430, 10) colorAccent FontLG "EF"
        mock `shouldHaveCalled` Mock_drawText (460, 550) colorLightGray FontLG "D"
        mock `shouldHaveCalled` Mock_drawText (470, 550) colorAccent FontLG "JK"

    context "when key sequence is complete match" $ do
      let currentState = defaultState {stateMode = ModeHints (modeHintsData $ stateMode defaultState) {stateKeySequence = "DEF"}}

      it "draws only the matching label" $ do
        (_, mock) <- runWithMocks $ do
          Mock_windowSize `mockReturns` mockWindowSize
          render currentState
        mock `shouldHaveCalled` Mock_drawText (1420, 10) colorLightGray FontLG "DEF"

  describe "#renderKeySequence" $ do
    context "when there is a partial match" $ do
      it "draws the matched section and highlights the remaining characters" $ do
        (_, mock) <- runWithMocks $ do
          Mock_windowSize `mockReturns` mockWindowSize
          Mock_drawText (0, 0) colorLightGray FontLG "ABC" `mockReturns` (30, 0)
          renderKeySequence "ABC" "ABCDE" (0, 0)
        mock `shouldHaveCalled` Mock_drawText (0, 0) colorLightGray FontLG "ABC"
        mock `shouldHaveCalled` Mock_drawText (30, 0) colorAccent FontLG "DE"

      it "return true as the text is visible" $ do
        (isVisible, _) <- runWithMocks $ renderKeySequence "ABC" "ABCDE" (0, 0)
        isVisible `shouldBe` True

    context "when there is no input key sequence" $ do
      it "draws text as a single chunk" $ do
        (_, mock) <- runWithMocks $ renderKeySequence "" "ABCD" (0, 0)
        mock `shouldHaveCalled` Mock_drawText (0, 0) colorWhite FontLG "ABCD"

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
