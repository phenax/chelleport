module Specs.AppStateUpdateSpec where

import Chelleport (update)
import Chelleport.Types
import Mock
import Test.Hspec

test = do
  describe "#update" $ do
    context "with action TriggerLeftClick" $ do
      let state = State {stateKeySequence = [], stateIsShiftPressed = False, stateIsMatched = False, stateGrid = []}

      it "hides window and triggers left clicks" $ do
        (_, mock) <- runWithMocks $ update state TriggerLeftClick
        calls mock `shouldContain` [CallHideWindow, CallPressMouseButton LeftClick]

      it "continues with action ShutdownApp without updating state" $ do
        ((state, action), mock) <- runWithMocks $ update state TriggerLeftClick
        action `shouldBe` Just ShutdownApp
        state `shouldBe` state
