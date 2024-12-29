module Specs.AppStateSpec where

import Chelleport.AppState (initialState, update)
import Chelleport.Types
import Chelleport.Utils (isNotEmpty, uniq)
import Control.Monad (join)
import Data.Default (Default (def))
import qualified SDL
import Test.Hspec
import TestUtils

test :: SpecWith ()
test = do
  let flush = pure ()
  describe "#initialState" $ do
    let config = def

    it "returns the initial state of the app" $ do
      ((initState, _), _) <- runWithMocks $ initialState config
      stateIsShiftPressed initState `shouldBe` False
      case stateMode initState of
        ModeHints hintsData -> do
          stateKeySequence hintsData `shouldBe` []
          stateIsMatched hintsData `shouldBe` False
        _ -> undefined

    it "returns grid with 16x9 key sequences" $ do
      ((initState, _), _) <- runWithMocks $ initialState config
      stateGridRows initState `shouldBe` 9
      stateGridCols initState `shouldBe` 16

    context "when config specifies mode" $ do
      let currentConfig = config {configMode = ModeSearch def}

      it "continues to set given mode" $ do
        ((_, action), _) <- runWithMocks $ initialState currentConfig
        action `shouldBe` Just (SetMode $ ModeSearch def)

  describe "#update" $ do
    let defaultHintModeData = def {stateGrid = [["ABC", "DEF"], ["DJK", "JKL"]]}
    let defaultState =
          def
            { stateGridRows = 2,
              stateGridCols = 2,
              stateMode = ModeHints defaultHintModeData
            }

    context "with action ChainMouseClick" $ do
      context "when repetition is 1" $ do
        let currentState = defaultState {stateRepetition = 1}

        it "hides window, triggers mouse click and shows the window again" $ do
          (_, mock) <- runWithMocks $ update flush currentState $ ChainMouseClick LeftClick
          mock `shouldContainCalls` [Mock_hideWindow, Mock_clickMouseButton LeftClick, Mock_showWindow]

        it "continues with action ResetKeys without updating state" $ do
          ((nextState, action), _) <- runWithMocks $ update flush currentState $ ChainMouseClick LeftClick
          action `shouldBe` Just ResetKeys
          nextState `shouldBe` currentState

      context "when repetition is more than 1" $ do
        let currentState = defaultState {stateRepetition = 3}

        it "resets repetition back to 1" $ do
          ((nextState, _), _) <- runWithMocks $ update flush currentState $ ChainMouseClick LeftClick
          nextState `shouldBe` currentState {stateRepetition = 1}

        it "clicks multiple times" $ do
          (_, mock) <- runWithMocks $ update flush currentState $ ChainMouseClick LeftClick
          mock
            `shouldContainCalls` [ Mock_hideWindow,
                                   Mock_clickMouseButton LeftClick,
                                   Mock_clickMouseButton LeftClick,
                                   Mock_clickMouseButton LeftClick,
                                   Mock_showWindow
                                 ]

    context "with action HandleFilterInputChange" $ do
      context "when mode is ModeSearch" $ do
        it "todo: implement" $ do
          True `shouldBe` True

      context "when mode is ModeHints" $ do
        context "when there are no matches" $ do
          let currentState = defaultState {stateMode = ModeHints $ defaultHintModeData {stateKeySequence = "DE"}}

          context "when input key sequence has matching values in grid" $ do
            it "does not update" $ do
              ((nextState, action), _) <- runWithMocks $ update flush currentState HandleFilterInputChange
              action `shouldBe` Nothing
              nextState `shouldBe` currentState

          context "when input key sequence does not have matching values in grid" $ do
            it "adds key to key sequence" $ do
              ((nextState, action), _) <- runWithMocks $ update flush currentState HandleFilterInputChange
              action `shouldBe` Nothing
              nextState `shouldBe` currentState {stateMode = ModeHints defaultHintModeData {stateIsMatched = False, stateKeySequence = "DE"}}

        context "when there are matches" $ do
          let currentState = defaultState {stateMode = ModeHints $ defaultHintModeData {stateKeySequence = "DEF"}}

          context "when input key sequence does not have matching values in grid" $ do
            it "adds key to key sequence and enables isMatched" $ do
              ((nextState, _), _) <- runWithMocks $ update flush currentState HandleFilterInputChange
              nextState `shouldBe` currentState {stateMode = ModeHints defaultHintModeData {stateIsMatched = True, stateKeySequence = "DEF"}}

            it "continues with MoveMousePosition action at center of matched cell" $ do
              ((_, action), _) <- runWithMocks $ do
                Mock_windowSize `mockReturns` mockWindowSize
                Mock_windowPosition `mockReturns` mockWindowPosition
                update flush currentState HandleFilterInputChange
              action `shouldBe` Just (MoveMousePosition (1640, 370))

    context "with action HandleKeyInput" $ do
      context "when mode is ModeSearch" $ do
        let currentState = defaultState {stateMode = ModeSearch $ def {searchInputText = "h"}}

        it "adds key to input text as lower cased and continues with filter action" $ do
          ((nextState, action), _) <- runWithMocks $ update flush currentState $ HandleKeyInput SDL.KeycodeE
          nextState `shouldBe` currentState {stateMode = ModeSearch def {searchInputText = "he"}}
          action `shouldBe` Just HandleFilterInputChange

      context "when mode is ModeHints" $ do
        let currentState = defaultState {stateMode = ModeHints $ defaultHintModeData {stateKeySequence = "D"}}

        context "when input key sequence has matching values in grid" $ do
          it "does not update" $ do
            ((nextState, action), _) <- runWithMocks $ update flush currentState $ HandleKeyInput SDL.KeycodeZ
            action `shouldBe` Nothing
            nextState `shouldBe` currentState

        context "when input key sequence does not have matching values in grid" $ do
          it "adds key to key sequence and continues with filter action" $ do
            ((nextState, action), _) <- runWithMocks $ update flush currentState $ HandleKeyInput SDL.KeycodeE
            nextState `shouldBe` currentState {stateMode = ModeHints defaultHintModeData {stateKeySequence = "DE"}}
            action `shouldBe` Just HandleFilterInputChange

    context "with action IncrementHighlightIndex" $ do
      context "when filtered match list is empty" $ do
        let defaultSearchData = def {searchFilteredWords = [], searchHighlightedIndex = 2}
        let currentState = defaultState {stateRepetition = 1, stateMode = ModeSearch defaultSearchData}

        it "does not continue or update state" $ do
          ((nextState, action), _) <- runWithMocks $ do
            update flush currentState $ IncrementHighlightIndex 3
          nextState `shouldBe` currentState {stateMode = ModeSearch defaultSearchData {searchHighlightedIndex = 0}}
          action `shouldBe` Nothing

      context "when there are some matches" $ do
        let defaultSearchData =
              def
                { searchHighlightedIndex = 0,
                  searchFilteredWords =
                    [ def {matchText = "Hello"},
                      def {matchStartX = 10, matchText = "World"},
                      def {matchStartX = 20, matchText = "Door"}
                    ]
                }
        let currentState = defaultState {stateRepetition = 2, stateMode = ModeSearch defaultSearchData}

        it "continues with moving mouse to word at index" $ do
          ((_, action), _) <- runWithMocks $ update flush currentState $ IncrementHighlightIndex 1
          action `shouldBe` Just (MoveMousePosition (fromIntegral mockWindowOffsetX + 20, fromIntegral mockWindowOffsetY))

        it "increments index by given number times the repetition count" $ do
          ((nextState, _), _) <- runWithMocks $ update flush currentState $ IncrementHighlightIndex 1
          nextState `shouldBe` currentState {stateRepetition = 1, stateMode = ModeSearch defaultSearchData {searchHighlightedIndex = 2}}

        it "resets repetition back to 1" $ do
          ((nextState, _), _) <- runWithMocks $ update flush currentState $ IncrementHighlightIndex 1
          stateRepetition nextState `shouldBe` 1

        context "when incrementing higher than the last element" $ do
          let defaultSearchData =
                def
                  { searchHighlightedIndex = 2,
                    searchFilteredWords =
                      [ def {matchText = "Hello"},
                        def {matchStartX = 10, matchText = "World"},
                        def {matchStartX = 20, matchText = "Door"}
                      ]
                  }
          let currentState = defaultState {stateRepetition = 2, stateMode = ModeSearch defaultSearchData}

          it "circles back to the start of match list" $ do
            ((nextState, _), _) <- runWithMocks $ do
              update flush currentState $ IncrementHighlightIndex 1
            nextState `shouldBe` currentState {stateRepetition = 1, stateMode = ModeSearch defaultSearchData {searchHighlightedIndex = 1}}

          it "continues with moving mouse to word at index" $ do
            ((_, action), _) <- runWithMocks $ do
              update flush currentState $ IncrementHighlightIndex 1
            action `shouldBe` Just (MoveMousePosition (fromIntegral mockWindowOffsetX + 10, fromIntegral mockWindowOffsetY))

    context "with action IncrementMouseCursor" $ do
      context "when repetition is 1" $ do
        let currentState = defaultState {stateRepetition = 1}

        it "continues with MoveMousePosition" $ do
          ((_, action), _) <- runWithMocks $ do
            Mock_getMousePointerPosition `mockReturns` (42, 42)
            update flush currentState $ IncrementMouseCursor (10, -5)
          action `shouldBe` Just (MoveMousePosition (52, 37))

        it "does update state" $ do
          ((state, _), _) <- runWithMocks $ update flush currentState $ IncrementMouseCursor (10, -5)
          state `shouldBe` currentState

      context "when repetition is more than 1" $ do
        let currentState = defaultState {stateRepetition = 5}

        it "multiplies increment" $ do
          ((_, action), _) <- runWithMocks $ do
            Mock_getMousePointerPosition `mockReturns` (42, 42)
            update flush currentState $ IncrementMouseCursor (10, -5)
          action `shouldBe` Just (MoveMousePosition (92, 17))

    context "with action MouseDragEnd" $ do
      let currentState = defaultState

      it "hides window, stops dragging and shows the window again" $ do
        (_, mock) <- runWithMocks $ update flush currentState MouseDragEnd
        mock `shouldContainCalls` [Mock_hideWindow, Mock_releaseMouseButton, Mock_showWindow]

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update flush currentState MouseDragStart
        result `shouldBe` (currentState, Nothing)

    context "with action MouseDragStart" $ do
      let currentState = defaultState

      it "hides window, starts dragging and shows the window again" $ do
        (_, mock) <- runWithMocks $ update flush currentState MouseDragStart
        mock `shouldContainCalls` [Mock_hideWindow, Mock_pressMouseButton, Mock_showWindow]

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update flush currentState MouseDragStart
        result `shouldBe` (currentState, Nothing)

    context "with action MouseDragToggle" $ do
      context "when is dragging is true" $ do
        let currentState = defaultState {stateIsDragging = True}

        it "toggles dragging state" $ do
          ((state, _), _) <- runWithMocks $ update flush currentState MouseDragToggle
          state `shouldBe` state {stateIsDragging = False}

        it "continues with action MouseDragEnd" $ do
          ((_, action), _) <- runWithMocks $ update flush currentState MouseDragToggle
          action `shouldBe` Just MouseDragEnd

      context "when is dragging is false" $ do
        let currentState = defaultState {stateIsDragging = False}

        it "toggles dragging state" $ do
          ((state, _), _) <- runWithMocks $ update flush currentState MouseDragToggle
          state `shouldBe` state {stateIsDragging = True}

        it "continues with action MouseDragStart" $ do
          ((_, action), _) <- runWithMocks $ update flush currentState MouseDragToggle
          action `shouldBe` Just MouseDragStart

    context "with action MoveMouseInDirection" $ do
      let currentState = defaultState

      context "when direction is up" $ do
        it "continues to increment movement" $ do
          ((_, action), _) <- runWithMocks $ do
            Mock_windowSize `mockReturns` mockWindowSize
            update flush currentState $ MoveMouseInDirection DirUp
          action `shouldBe` Just (IncrementMouseCursor (0, -33))

      context "when direction is down" $ do
        it "continues to increment movement" $ do
          ((_, action), _) <- runWithMocks $ do
            Mock_windowSize `mockReturns` mockWindowSize
            update flush currentState $ MoveMouseInDirection DirDown
          action `shouldBe` Just (IncrementMouseCursor (0, 33))

      context "when direction is left" $ do
        it "continues to increment movement" $ do
          ((_, action), _) <- runWithMocks $ do
            Mock_windowSize `mockReturns` mockWindowSize
            update flush currentState $ MoveMouseInDirection DirLeft
          action `shouldBe` Just (IncrementMouseCursor (-60, 0))

      context "when direction is right" $ do
        it "continues to increment movement" $ do
          ((_, action), _) <- runWithMocks $ do
            Mock_windowSize `mockReturns` mockWindowSize
            update flush currentState $ MoveMouseInDirection DirRight
          action `shouldBe` Just (IncrementMouseCursor (60, 0))

    context "with action MoveMousePosition" $ do
      let currentState = defaultState

      it "moves mouse pointer to the given coordinates" $ do
        (_, mock) <- runWithMocks $ update flush currentState $ MoveMousePosition (23, 320)
        mock `shouldHaveCalled` Mock_moveMousePointer 23 320

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update flush currentState $ MoveMousePosition (0, 0)
        result `shouldBe` (currentState, Nothing)

    context "with action ResetKeys" $ do
      let currentState = defaultState {stateRepetition = 5}

      it "resets state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update flush currentState ResetKeys
        action `shouldBe` Nothing
        nextState
          `shouldBe` currentState
            { stateMode = ModeHints $ defaultHintModeData {stateKeySequence = [], stateIsMatched = False},
              stateRepetition = 1
            }

    context "with action SetMode" $ do
      let currentState = defaultState

      it "updates mode in state and continues to initialize mode" $ do
        ((nextState, action), _) <- runWithMocks $ update flush currentState $ SetMode $ ModeHints def
        nextState `shouldBe` currentState {stateMode = ModeHints def, stateIsModeInitialized = False}
        action `shouldBe` Just InitializeMode

    context "with action InitializeMode" $ do
      context "when mode is ModeHints" $ do
        let rows = 13
        let cols = 11
        let currentState =
              defaultState
                { stateGridRows = rows,
                  stateGridCols = cols,
                  stateMode = ModeHints defaultHintModeData,
                  stateIsModeInitialized = False
                }

        it "updates initialization state to true" $ do
          ((nextState, action), _) <- runWithMocks $ update flush currentState InitializeMode
          stateIsModeInitialized nextState `shouldBe` True
          action `shouldBe` Nothing

        it "returns grid of key sequences with the right number of rows and columns" $ do
          ((nextState, _), _) <- runWithMocks $ update flush currentState InitializeMode
          case stateMode nextState of
            ModeHints hintsData -> do
              length (stateGrid hintsData) `shouldBe` rows
              stateGrid hintsData `shouldSatisfy` all ((== cols) . length)
              stateGrid hintsData `shouldSatisfy` all (all ((== 2) . length))
            _ -> undefined

        it "returns grid with all unique key sequences" $ do
          ((nextState, _), _) <- runWithMocks $ update flush currentState InitializeMode
          case stateMode nextState of
            ModeHints hintsData -> do
              join (stateGrid hintsData) `shouldSatisfy` isNotEmpty
              join (stateGrid hintsData) `shouldBe` uniq (join $ stateGrid hintsData)
            _ -> undefined

      context "when mode is ModeSearch" $ do
        let currentState = defaultState {stateMode = ModeSearch def, stateIsModeInitialized = False}

        it "captures screenshot for word search" $ do
          ((_, _), mock) <- runWithMocks $ do
            Mock_windowSize `mockReturns` mockWindowSize
            Mock_windowPosition `mockReturns` mockWindowPosition
            update flush currentState InitializeMode
          mock `shouldHaveCalled` Mock_captureScreenshot (mockWindowOffsetX, mockWindowOffsetY) (mockWindowWidth, mockWindowHeight)

        it "updates mode in state with ocr words" $ do
          let matchWord = OCRMatch {matchStartX = 40, matchStartY = 5, matchEndX = 100, matchEndY = 20, matchText = "Wow"}
          ((nextState, _), _) <- runWithMocks $ do
            Mock_windowSize `mockReturns` mockWindowSize
            Mock_windowPosition `mockReturns` mockWindowPosition
            Mock_captureScreenshot mockWindowPosition mockWindowSize `mockReturns` "mock-filename"
            Mock_getWordsInImage "mock-filename" `mockReturns` [matchWord]
            update flush currentState InitializeMode
          nextState
            `shouldBe` currentState
              { stateIsModeInitialized = True,
                stateMode =
                  ModeSearch $
                    def
                      { searchWords = [matchWord],
                        searchFilteredWords = [matchWord]
                      }
              }

    context "with action ShutdownApp" $ do
      let currentState = defaultState

      it "shuts down app" $ do
        (_, mock) <- runWithMocks $ update flush currentState ShutdownApp
        mock `shouldHaveCalled` Mock_shutdownApp

      it "does not continue or update state" $ do
        (result, _) <- runWithMocks $ update flush currentState ShutdownApp
        result `shouldBe` (currentState, Nothing)

    context "with action TriggerMouseClick" $ do
      context "when repetition is 1" $ do
        let currentState = defaultState {stateRepetition = 1}

        it "hides window and triggers mouse click" $ do
          (_, mock) <- runWithMocks $ update flush currentState $ TriggerMouseClick LeftClick
          mock `shouldContainCalls` [Mock_hideWindow, Mock_clickMouseButton LeftClick]

        it "continues with action ShutdownApp without updating state" $ do
          ((nextState, action), _) <- runWithMocks $ update flush currentState $ TriggerMouseClick LeftClick
          action `shouldBe` Just ShutdownApp
          nextState `shouldBe` currentState

      context "when repetition is more than 1" $ do
        let currentState = defaultState {stateRepetition = 3}

        it "resets repetition back to 1" $ do
          ((nextState, _), _) <- runWithMocks $ update flush currentState $ TriggerMouseClick LeftClick
          nextState `shouldBe` currentState {stateRepetition = 1}

        it "clicks multiple times" $ do
          (_, mock) <- runWithMocks $ update flush currentState $ TriggerMouseClick LeftClick
          mock
            `shouldContainCalls` [ Mock_hideWindow,
                                   Mock_clickMouseButton LeftClick,
                                   Mock_clickMouseButton LeftClick,
                                   Mock_clickMouseButton LeftClick
                                 ]

    context "with action UpdateRepetition" $ do
      let currentState = defaultState

      it "updates repetition without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update flush currentState $ UpdateRepetition 7
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateRepetition = 7}

      context "when count is 0" $ do
        it "updates repetition to 1" $ do
          ((nextState, action), _) <- runWithMocks $ update flush currentState $ UpdateRepetition 0
          action `shouldBe` Nothing
          nextState `shouldBe` currentState {stateRepetition = 1}

    context "with action UpdateShiftState" $ do
      let currentState = defaultState

      it "updates shift state without any action" $ do
        ((nextState, action), _) <- runWithMocks $ update flush currentState $ UpdateShiftState True
        action `shouldBe` Nothing
        nextState `shouldBe` currentState {stateIsShiftPressed = True}
