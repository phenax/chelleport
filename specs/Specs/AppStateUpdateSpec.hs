module Specs.AppStateUpdateSpec where

import Chelleport (initialState, update)
import Chelleport.Types
import Chelleport.Utils (uniq)
import Control.Monad (join)
import Mock
import qualified SDL
import Test.Hspec

test :: SpecWith ()
test = do
  describe "#initialState" $ do
    it "returns the initial state of the app" $ do
      (initState, _) <- runWithMocks initialState
      stateKeySequence initState `shouldBe` []
      stateIsMatched initState `shouldBe` False
      stateIsShiftPressed initState `shouldBe` False

    it "returns grid with 16x9 key sequences" $ do
      (initState, _) <- runWithMocks initialState
      length (stateGrid initState) `shouldBe` 9
      stateGrid initState `shouldSatisfy` all ((== 16) . length)
      stateGrid initState `shouldSatisfy` all (all ((== 2) . length))

    it "returns grid with all unique key sequences" $ do
      (initState, _) <- runWithMocks initialState
      join (stateGrid initState) `shouldBe` uniq (join $ stateGrid initState)

  describe "#update" $ do
    let defaultState =
          State
            { stateKeySequence = [],
              stateIsShiftPressed = False,
              stateIsMatched = False,
              stateGrid = [["ABC", "DEF"], ["DJK", "JKL"]]
            }

    context "with action HandleKeyInput" $ do
      context "when there are no matches" $ do
        let currentState = defaultState {stateKeySequence = "D"}

        context "when input key sequence has matching values in grid" $ do
          it "does not update" $ do
            ((nextState, action), _) <- runWithMocks $ update currentState $ HandleKeyInput SDL.KeycodeZ
            action `shouldBe` Nothing
            nextState `shouldBe` currentState

        context "when input key sequence does not have matching values in grid" $ do
          it "adds key to key sequence" $ do
            ((nextState, action), _) <- runWithMocks $ update currentState $ HandleKeyInput SDL.KeycodeE
            action `shouldBe` Nothing
            nextState `shouldBe` currentState {stateKeySequence = "DE"}

      context "when there is a matches" $ do
        let currentState = defaultState {stateKeySequence = "DE"}

        context "when input key sequence does not have matching values in grid" $ do
          it "adds key to key sequence and enables isMatched" $ do
            ((nextState, _), _) <- runWithMocks $ update currentState $ HandleKeyInput SDL.KeycodeF
            nextState `shouldBe` currentState {stateKeySequence = "DEF", stateIsMatched = True}

          it "continues with MoveMousePosition action" $ do
            ((_, action), _) <- runWithMocks $ update currentState $ HandleKeyInput SDL.KeycodeF
            action `shouldBe` Just (MoveMousePosition (0, 1))

    context "with action TriggerMouseClick" $ do
      let currentState = defaultState

      it "hides window and triggers mouse click" $ do
        (_, mock) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
        calls mock `shouldContain` [CallHideWindow, CallPressMouseButton LeftClick]

      it "continues with action ShutdownApp without updating state" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
        action `shouldBe` Just ShutdownApp
        nextState `shouldBe` currentState

    context "with action ChainMouseClick" $ do
      let currentState = defaultState

      it "hides window, triggers mouse click and shows the window again" $ do
        (_, mock) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
        calls mock `shouldBe` [CallHideWindow, CallPressMouseButton LeftClick, CallShowWindow]

      it "continues with action ResetKeys without updating state" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
        action `shouldBe` Just ResetKeys
        nextState `shouldBe` currentState

    context "with action MoveMousePosition" $ do
      let currentState = defaultState

      -- TODO: Test with inline mocked values
      it "moves mouse pointer to center of cell of given coordinates" $ do
        (_, mock) <- runWithMocks $ update currentState $ MoveMousePosition (0, 0)
        -- handleMocks
        --   [ CallPressMouseButton LeftClick `returns` (1, 2),
        --     CallHideWindow `returns` ()
        --   ]
        mock `shouldHaveCalled` CallMoveMousePosition 25 25

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState $ MoveMousePosition (0, 0)
        result `shouldBe` (currentState, Nothing)

    context "with action ResetKeys" $ do
      let currentState = defaultState

      it "resets state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState ResetKeys
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateKeySequence = [], stateIsMatched = False}

    context "with action IncrementMouseCursor" $ do
      let currentState = defaultState

      -- TODO: Test with inline mocked values
      it "increments mouse position relative to current position" $ do
        (_, mock) <- runWithMocks $ update currentState $ IncrementMouseCursor (10, -20)
        mock `shouldHaveCalled` CallMoveMousePosition 52 22

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState $ IncrementMouseCursor (0, 0)
        result `shouldBe` (currentState, Nothing)

    context "with action ShutdownApp" $ do
      let currentState = defaultState

      it "shuts down app" $ do
        (_, mock) <- runWithMocks $ update currentState ShutdownApp
        mock `shouldHaveCalled` CallShutdownApp

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState ShutdownApp
        result `shouldBe` (currentState, Nothing)

    context "with action UpdateShiftState" $ do
      let currentState = defaultState

      it "updates shift state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ UpdateShiftState True
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateIsShiftPressed = True}
