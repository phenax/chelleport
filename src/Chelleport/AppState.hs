module Chelleport.AppState (initialState, update) where

import Chelleport.AppShell (MonadAppShell (hideWindow, showWindow, shutdownApp), Update)
import Chelleport.Control (MonadControl (..), directionalIncrement, hjklDirection)
import Chelleport.Draw (MonadDraw (windowPosition, windowSize), pointerPositionIncrement, screenPositionFromCellPosition, wordPosition)
import Chelleport.KeySequence (findMatchPosition, generateGrid, nextChars, toKeyChar)
import Chelleport.OCR (MonadOCR (captureScreenshot), getWordsInImage)
import Chelleport.Types
import Chelleport.Utils (cIntToInt, clamp, cycleInRange, intToCInt, isEmpty, itemAt)
import Control.Monad (replicateM_)
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Maybe (isJust)
import qualified Text.Fuzzy as Fuzzy

initialState :: (Monad m) => Configuration -> m (State, Maybe AppAction)
initialState config = do
  let action = Just $ SetMode $ configMode config
  pure (def {stateGridRows = rows, stateGridCols = columns}, action)
  where
    rows = 9
    columns = 16

update :: (MonadAppShell m, MonadDraw m, MonadControl m, MonadOCR m) => Update m State AppAction
-- Chain clicks
update _ state (ChainMouseClick btn) = do
  hideWindow
  replicateM_ (stateRepetition state) $ clickMouseButton btn
  showWindow
  pure (state {stateRepetition = 1}, Just ResetKeys)

-- Delete last input char
update _ state DeleteLastInput = case stateMode state of
  ModeHints {} -> pure (state, Nothing)
  ModeSearch searchData@(ModeSearchData {searchInputText})
    | isEmpty searchInputText -> pure (state, Nothing)
    | otherwise -> do
        let updatedText = take (length searchInputText - 1) searchInputText
        pure
          ( state {stateMode = ModeSearch searchData {searchInputText = updatedText}},
            Just HandleFilterInputChange
          )

-- HINTS MODE: Set match state when a match is found for the key sequence
update _ state@(State {stateMode = ModeHints hintsData}) HandleFilterInputChange = do
  let updatedHintsData = hintsData {stateIsMatched = isJust matchPosition}
  action <- fmap MoveMousePosition <$> traverse (screenPositionFromCellPosition state) matchPosition
  pure (state {stateMode = ModeHints updatedHintsData}, action)
  where
    (ModeHintsData {stateKeySequence}) = hintsData
    matchPosition = findMatchPosition stateKeySequence $ stateGrid hintsData

-- SEARCH MODE: Filter results based on text input
update _ state@(State {stateMode = ModeSearch searchData}) HandleFilterInputChange = do
  let updatedModeData =
        searchData
          { searchFilteredWords = filteredMatches,
            searchHighlightedIndex = highlightedIndex
          }
  action <- fmap MoveMousePosition <$> traverse wordPosition highlightedWord
  pure (state {stateMode = ModeSearch updatedModeData}, action)
  where
    (ModeSearchData {searchInputText, searchWords, searchHighlightedIndex}) = searchData
    highlightedIndex = clamp (0, length filteredMatches - 1) searchHighlightedIndex
    highlightedWord = filteredMatches `itemAt` highlightedIndex
    filteredMatches
      | isEmpty searchInputText = searchWords
      | otherwise = Fuzzy.original <$> Fuzzy.filter searchInputText searchWords "" "" matchText False

-- HINTS MODE: Act on key inputs
update _ state@(State {stateMode = ModeHints hintsData}) (HandleKeyInput keycode) = do
  case (toKeyChar keycode, validNextKeys) of
    (Just keyChar, Just validChars')
      | stateIsMatched hintsData && keyChar `elem` ("HJKL" :: String) -> do
          pure (state, Just $ MoveMouseInDirection $ hjklDirection keyChar)
      | keyChar `elem` validChars' -> do
          let updatedHintsData = hintsData {stateKeySequence = stateKeySequence hintsData ++ [keyChar]}
          pure (state {stateMode = ModeHints updatedHintsData}, Just HandleFilterInputChange)
    _ -> pure (state, Nothing)
  where
    validNextKeys = nextChars (stateKeySequence hintsData) (stateGrid hintsData)

-- SEARCH MODE: Act on key inputs
update _ state@(State {stateMode = ModeSearch searchData}) (HandleKeyInput keycode) = do
  case toKeyChar keycode of
    Just keyChar -> do
      let updatedMode = searchData {searchInputText = searchInputText searchData ++ [toLower keyChar]}
      pure (state {stateMode = ModeSearch updatedMode}, Just HandleFilterInputChange)
    _ -> pure (state, Nothing)

-- Increment highlighted index for search mode
update _ state (IncrementHighlightIndex n) = do
  case stateMode state of
    ModeSearch searchData@(ModeSearchData {searchFilteredWords, searchHighlightedIndex}) -> do
      let updatedModeData = searchData {searchHighlightedIndex = updatedHighlightedIndex}
      action <- traverse (fmap MoveMousePosition . wordPosition) highlightedWord
      pure (state {stateRepetition = 1, stateMode = ModeSearch updatedModeData}, action)
      where
        highlightedWord = searchFilteredWords `itemAt` updatedHighlightedIndex
        increment = n * stateRepetition state
        updatedHighlightedIndex =
          cycleInRange (0, length searchFilteredWords - 1) $ searchHighlightedIndex + increment
    _ -> pure (state, Nothing)

-- Move mouse incrementally
update _ state (IncrementMouseCursor (incX, incY)) = do
  (curX, curY) <- getMousePointerPosition
  let count = stateRepetition state
  let pos = (cIntToInt curX + count * incX, cIntToInt curY + count * incY)
  pure (state {stateRepetition = 1}, Just $ MoveMousePosition pos)

-- Mouse button release
update _ state MouseDragEnd = do
  hideWindow
  releaseMouseButton
  showWindow
  pure (state {stateRepetition = 1}, Nothing)

-- Mouse button press
update _ state MouseDragStart = do
  hideWindow
  pressMouseButton
  showWindow
  pure (state {stateRepetition = 1}, Nothing)

-- Mouse dragging
update _ state MouseDragToggle
  | stateIsDragging state = pure (state {stateIsDragging = False}, Just MouseDragEnd)
  | otherwise = do pure (state {stateIsDragging = True}, Just MouseDragStart)

-- Apply movement in given direction
update _ state (MoveMouseInDirection direction) = do
  incr <- pointerPositionIncrement state
  pure (state, Just $ IncrementMouseCursor $ directionalIncrement incr direction)

-- Move mouse to given position
update _ state (MoveMousePosition (x, y)) = do
  moveMousePointer (intToCInt x) (intToCInt y)
  pure (state, Nothing)

-- Reset entered key sequence and state
update _ state ResetKeys = do
  let nextState =
        state
          { stateRepetition = 1,
            stateMode = resetMode (stateMode state)
          }
  pure (nextState, Nothing)
  where
    resetMode (ModeHints hintsData) = ModeHints $ hintsData {stateKeySequence = [], stateIsMatched = False}
    resetMode (ModeSearch searchData@(ModeSearchData {searchWords})) =
      ModeSearch $ searchData {searchWords = searchWords, searchFilteredWords = searchWords, searchInputText = ""}

-- Initialize current mode
update flush state InitializeMode =
  case stateMode state of
    ModeHints hintsData -> do
      let cells = either error id $ generateGrid 0 (stateGridRows state, stateGridCols state) ['A' .. 'Z']
      let updateHintsData = hintsData {stateGrid = cells}
      pure (state {stateMode = ModeHints updateHintsData, stateIsModeInitialized = True}, Nothing)
    ModeSearch searchData -> do
      position <- windowPosition
      size <- windowSize
      hideWindow
      screenshot <- captureScreenshot position size
      showWindow
      flush
      matches <- getWordsInImage screenshot
      let updatedSearchData = searchData {searchWords = matches, searchFilteredWords = matches, searchInputText = ""}
      pure (state {stateMode = ModeSearch updatedSearchData, stateIsModeInitialized = True}, Nothing)

-- Set mode
update _ state (SetMode mode) = do
  pure (state {stateMode = mode, stateIsModeInitialized = False}, Just InitializeMode)

-- Cleanup everything and exit
update _ state ShutdownApp = do
  shutdownApp
  pure (state, Nothing)

-- Trigger click
update _ state (TriggerMouseClick btn) = do
  hideWindow
  replicateM_ (stateRepetition state) $ clickMouseButton btn
  pure (state {stateRepetition = 1}, Just ShutdownApp)

-- Set repetition count
update _ state (UpdateRepetition count) = do
  pure (state {stateRepetition = max 1 count}, Nothing)

-- Set/unset whether shift is pressed
update _ state (UpdateShiftState shiftPressed) =
  pure (state {stateIsShiftPressed = shiftPressed}, Nothing)
