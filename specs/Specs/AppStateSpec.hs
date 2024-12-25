module Specs.AppStateSpec where

import Chelleport.AppState (initialState, update)
import Chelleport.Types
import Chelleport.Utils (uniq)
import Control.Monad (join)
import qualified SDL
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  describe "#initialState" $ do
    it "returns the initial state of the app" $ do
      ((initState, _), _) <- runWithMocks initialState
      stateKeySequence initState `shouldBe` []
      stateIsMatched initState `shouldBe` False
      stateIsShiftPressed initState `shouldBe` False

    it "returns grid with 16x9 key sequences" $ do
      ((initState, _), _) <- runWithMocks initialState
      length (stateGrid initState) `shouldBe` 9
      stateGrid initState `shouldSatisfy` all ((== 16) . length)
      stateGrid initState `shouldSatisfy` all (all ((== 2) . length))

    it "returns grid with all unique key sequences" $ do
      ((initState, _), _) <- runWithMocks initialState
      join (stateGrid initState) `shouldBe` uniq (join $ stateGrid initState)

  describe "#update" $ do
    let defaultState = defaultAppState {stateGrid = [["ABC", "DEF"], ["DJK", "JKL"]]}

    context "with action ChainMouseClick" $ do
      let currentState = defaultState

      it "hides window, triggers mouse click and shows the window again" $ do
        (_, mock) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
        calls mock `shouldBe` [Mock_hideWindow, Mock_clickMouseButton LeftClick, Mock_showWindow]

      it "continues with action ResetKeys without updating state" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
        action `shouldBe` Just ResetKeys
        nextState `shouldBe` currentState

      context "when repetition is more than 1" $ do
        let currentState = defaultState {stateRepetition = 3}

        it "resets repetition back to 1" $ do
          ((nextState, _), _) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
          nextState `shouldBe` currentState {stateRepetition = 1}

        it "clicks multiple times" $ do
          (_, mock) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
          calls mock
            `shouldBe` [ Mock_hideWindow,
                         Mock_clickMouseButton LeftClick,
                         Mock_clickMouseButton LeftClick,
                         Mock_clickMouseButton LeftClick,
                         Mock_showWindow
                       ]

      context "when repetition is 0" $ do
        let currentState = defaultState {stateRepetition = 0}

        it "clicks just once" $ do
          (_, mock) <- runWithMocks $ update currentState $ ChainMouseClick LeftClick
          calls mock `shouldBe` [Mock_hideWindow, Mock_clickMouseButton LeftClick, Mock_showWindow]

    context "with action IncrementHighlightIndex" $ do
      let currentState = defaultState

      it "todo: implement" $ do
        1 `shouldBe` 1

    context "with action IncrementMouseCursor" $ do
      let currentState = defaultState

      it "continues with MoveMousePosition" $ do
        ((_, action), _) <- runWithMocks $ update currentState $ IncrementMouseCursor (10, -5)
        action `shouldBe` Just (MoveMousePosition (52, 37))

      it "does update state" $ do
        ((state, _), _) <- runWithMocks $ update currentState $ IncrementMouseCursor (10, -5)
        state `shouldBe` currentState

      context "when repetition is more than 1" $ do
        let currentState = defaultState {stateRepetition = 5}

        it "multiplies increment" $ do
          ((_, action), _) <- runWithMocks $ update currentState $ IncrementMouseCursor (10, -5)
          action `shouldBe` Just (MoveMousePosition (92, 17))

      context "when repetition is 0" $ do
        let currentState = defaultState {stateRepetition = 0}

        it "increments just once" $ do
          ((_, action), _) <- runWithMocks $ update currentState $ IncrementMouseCursor (10, -5)
          action `shouldBe` Just (MoveMousePosition (52, 37))

    context "with action MouseDragEnd" $ do
      let currentState = defaultState

      it "hides window, stops dragging and shows the window again" $ do
        (_, mock) <- runWithMocks $ update currentState MouseDragEnd
        calls mock `shouldContain` [Mock_hideWindow, Mock_releaseMouseButton, Mock_showWindow]

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState MouseDragStart
        result `shouldBe` (currentState, Nothing)

    context "with action MouseDragStart" $ do
      let currentState = defaultState

      it "hides window, starts dragging and shows the window again" $ do
        (_, mock) <- runWithMocks $ update currentState MouseDragStart
        calls mock `shouldContain` [Mock_hideWindow, Mock_pressMouseButton, Mock_showWindow]

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState MouseDragStart
        result `shouldBe` (currentState, Nothing)

    context "with action MouseDragToggle" $ do
      context "when is dragging is true" $ do
        let currentState = defaultState {stateIsDragging = True}

        it "toggles dragging state" $ do
          ((state, _), _) <- runWithMocks $ update currentState MouseDragToggle
          state `shouldBe` state {stateIsDragging = False}

        it "continues with action MouseDragEnd" $ do
          ((_, action), _) <- runWithMocks $ update currentState MouseDragToggle
          action `shouldBe` Just MouseDragEnd

      context "when is dragging is false" $ do
        let currentState = defaultState {stateIsDragging = False}

        it "toggles dragging state" $ do
          ((state, _), _) <- runWithMocks $ update currentState MouseDragToggle
          state `shouldBe` state {stateIsDragging = True}

        it "continues with action MouseDragStart" $ do
          ((_, action), _) <- runWithMocks $ update currentState MouseDragToggle
          action `shouldBe` Just MouseDragStart

    context "with action HandleKeyInput" $ do
      context "when mode is ModeSearch" $ do
        it "todo: implement" $ do
          1 `shouldBe` 1

      context "when mode is ModeHints" $ do
        context "when there are no matches" $ do
          let currentState = defaultState {stateKeySequence = "D", stateMode = defaultHintsMode}

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
          let currentState = defaultState {stateKeySequence = "DE", stateMode = defaultHintsMode}

          context "when input key sequence does not have matching values in grid" $ do
            it "adds key to key sequence and enables isMatched" $ do
              ((nextState, _), _) <- runWithMocks $ update currentState $ HandleKeyInput SDL.KeycodeF
              nextState `shouldBe` currentState {stateKeySequence = "DEF", stateIsMatched = True}

            it "continues with MoveMousePosition action at center of matched cell" $ do
              ((_, action), _) <- runWithMocks $ update currentState $ HandleKeyInput SDL.KeycodeF
              action `shouldBe` Just (MoveMousePosition (1640, 370))

    context "with action MoveMousePosition" $ do
      let currentState = defaultState

      it "moves mouse pointer to the given coordinates" $ do
        (_, mock) <- runWithMocks $ update currentState $ MoveMousePosition (23, 320)
        mock `shouldHaveCalled` Mock_moveMousePointer 23 320

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState $ MoveMousePosition (0, 0)
        result `shouldBe` (currentState, Nothing)

    context "with action ResetKeys" $ do
      let currentState = defaultState {stateRepetition = 5}

      it "resets state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState ResetKeys
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateKeySequence = [], stateIsMatched = False, stateRepetition = 1}

    context "with action SetMode" $ do
      let currentState = defaultState

      context "when mode is ModeHints" $ do
        it "updates mode in state" $ do
          ((nextState, action), _) <- runWithMocks $ update currentState $ SetMode defaultHintsMode
          nextState `shouldBe` currentState {stateMode = defaultHintsMode}
          action `shouldBe` Nothing

      context "when mode is ModeSearch" $ do
        it "captures screenshot for word search" $ do
          ((_, _), mock) <- runWithMocks $ update currentState $ SetMode defaultSearchMode
          mock `shouldHaveCalled` Mock_captureScreenshot (mockWindowOffsetX, mockWindowOffsetY) (mockWindowWidth, mockWindowHeight)

        it "updates mode in state with ocr words" $ do
          ((nextState, _), _) <- runWithMocks $ update currentState $ SetMode defaultSearchMode
          let matchWord = OCRMatch {matchStartX = 40, matchStartY = 5, matchEndX = 100, matchEndY = 20, matchText = "Wow"}
          nextState `shouldBe` currentState {stateMode = defaultSearchMode {searchWords = [matchWord], searchFilteredWords = [matchWord]}}

    context "with action ShutdownApp" $ do
      let currentState = defaultState

      it "shuts down app" $ do
        (_, mock) <- runWithMocks $ update currentState ShutdownApp
        mock `shouldHaveCalled` Mock_shutdownApp

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update currentState ShutdownApp
        result `shouldBe` (currentState, Nothing)

    context "with action TriggerMouseClick" $ do
      let currentState = defaultState

      it "hides window and triggers mouse click" $ do
        (_, mock) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
        calls mock `shouldContain` [Mock_hideWindow, Mock_clickMouseButton LeftClick]

      it "continues with action ShutdownApp without updating state" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
        action `shouldBe` Just ShutdownApp
        nextState `shouldBe` currentState

      context "when repetition is more than 1" $ do
        let currentState = defaultState {stateRepetition = 3}

        it "resets repetition back to 1" $ do
          ((nextState, _), _) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
          nextState `shouldBe` currentState {stateRepetition = 1}

        it "clicks multiple times" $ do
          (_, mock) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
          calls mock
            `shouldBe` [ Mock_hideWindow,
                         Mock_clickMouseButton LeftClick,
                         Mock_clickMouseButton LeftClick,
                         Mock_clickMouseButton LeftClick
                       ]

      context "when repetition is 0" $ do
        let currentState = defaultState {stateRepetition = 0}

        it "clicks just once" $ do
          (_, mock) <- runWithMocks $ update currentState $ TriggerMouseClick LeftClick
          calls mock `shouldBe` [Mock_hideWindow, Mock_clickMouseButton LeftClick]

    context "with action UpdateRepetition" $ do
      let currentState = defaultState

      it "updates shift state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ UpdateRepetition 7
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateRepetition = 7}

    context "with action UpdateShiftState" $ do
      let currentState = defaultState

      it "updates shift state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update currentState $ UpdateShiftState True
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateIsShiftPressed = True}
