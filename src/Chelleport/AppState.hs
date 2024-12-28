module Chelleport.AppState (initialState, update) where

import Chelleport.AppShell (MonadAppShell (hideWindow, showWindow, shutdownApp), Update)
import Chelleport.Control (MonadControl (..), directionalIncrement, hjklDirection)
import Chelleport.Draw (MonadDraw (windowPosition, windowSize), pointerPositionIncrement, screenPositionFromCellPosition, wordPosition)
import Chelleport.KeySequence (findMatchPosition, generateGrid, nextChars, toKeyChar)
import Chelleport.OCR (MonadOCR (captureScreenshot), getWordsInImage)
import Chelleport.Types
import Chelleport.Utils (cIntToInt, clamp, intToCInt, isEmpty, itemAt)
import Control.Monad (replicateM_)
import Data.Char (toLower)
import Data.Default (Default (def))
import Data.Maybe (isJust)
import qualified Text.Fuzzy as Fuzzy

initialState :: (Monad m) => Configuration -> m (State, Maybe AppAction)
initialState config = do
  let cells = either error id $ generateGrid 0 (rows, columns) hintKeys
  let action = Just $ SetMode $ configMode config
  pure (def {stateGrid = cells}, action)
  where
    rows = 9
    columns = 16
    hintKeys = ['A' .. 'Z']

update :: (MonadAppShell m, MonadDraw m, MonadControl m, MonadOCR m) => Update m State AppAction
-- Chain clicks
update _ state (ChainMouseClick btn) = do
  hideWindow
  replicateM_ (stateRepetition state) $ clickMouseButton btn
  showWindow
  pure (state {stateRepetition = 1}, Just ResetKeys)

-- HINTS MODE: Act on key inputs
update _ state@(State {stateMode = ModeHints}) (HandleKeyInput keycode) = do
  case (toKeyChar keycode, validNextKeys) of
    (Just keyChar, Just validChars')
      | stateIsMatched state && keyChar `elem` ("HJKL" :: String) -> do
          pure (state, Just $ MoveMouseInDirection $ hjklDirection keyChar)
      | keyChar `elem` validChars' -> do
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let matchPosition = findMatchPosition newKeySequence $ stateGrid state
          let state' = state {stateKeySequence = newKeySequence, stateIsMatched = isJust matchPosition}
          action <- traverse (fmap MoveMousePosition . screenPositionFromCellPosition state) matchPosition
          pure (state', action)
    _ -> pure (state, Nothing)
  where
    validNextKeys = nextChars (stateKeySequence state) (stateGrid state)

-- SEARCH MODE: Act on key inputs
update _ state@(State {stateMode = ModeSearch {searchWords, searchInputText}}) (HandleKeyInput keycode) = do
  case toKeyChar keycode of
    Just keyChar -> do
      let searchText = searchInputText ++ [toLower keyChar]
      let matches = filterMatches searchText
      let highlightedIndex = clamp (0, length matches - 1) (searchHighlightedIndex mode)
      let updatedMode =
            mode
              { searchInputText = searchText,
                searchFilteredWords = matches,
                searchHighlightedIndex = highlightedIndex
              }
      let highlightedWord = matches `itemAt` highlightedIndex
      action <- traverse (fmap MoveMousePosition . wordPosition) highlightedWord
      pure (state {stateMode = updatedMode}, action)
    _ -> do
      pure (state, Nothing)
  where
    mode = stateMode state
    filterMatches text
      | isEmpty text = searchWords
      | otherwise = Fuzzy.original <$> Fuzzy.filter text searchWords "" "" matchText False

-- Increment highlighted index for search mode
update _ state (IncrementHighlightIndex n) = do
  case stateMode state of
    ModeSearch {} -> do
      action <- traverse (fmap MoveMousePosition . wordPosition) highlightedWord
      pure (state {stateRepetition = 1, stateMode = mode {searchHighlightedIndex = highlightedIndexClamped}}, action)
      where
        highlightedWord = searchFilteredWords mode `itemAt` highlightedIndexClamped
        highlightedIndex = searchHighlightedIndex mode + n
        highlightedIndexClamped =
          if highlightedIndex < 0
            then length (searchFilteredWords mode) - 1
            else highlightedIndex `mod` length (searchFilteredWords mode)
        mode = stateMode state
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
  pure
    ( state
        { stateKeySequence = [],
          stateIsMatched = False,
          stateRepetition = 1,
          stateMode = resetMode (stateMode state)
        },
      Nothing
    )
  where
    resetMode mode@ModeHints = mode
    resetMode (ModeSearch {searchWords}) =
      defaultSearchMode {searchWords = searchWords, searchFilteredWords = searchWords}

-- Initialize current mode
update flush state InitializeMode =
  case stateMode state of
    ModeHints -> pure (state {stateIsModeInitialized = True}, Nothing)
    ModeSearch {} -> do
      position <- windowPosition
      size <- windowSize
      hideWindow
      screenshot <- captureScreenshot position size
      showWindow
      flush
      matches <- getWordsInImage screenshot
      let updatedMode = (stateMode state) {searchWords = matches, searchFilteredWords = matches}
      pure (state {stateMode = updatedMode, stateIsModeInitialized = True}, Nothing)

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
