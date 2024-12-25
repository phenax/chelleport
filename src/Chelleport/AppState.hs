module Chelleport.AppState (initialState, update) where

import Chelleport.AppShell (MonadAppShell (hideWindow, showWindow, shutdownApp))
import Chelleport.Control (MonadControl (..), directionalIncrement)
import Chelleport.Draw (MonadDraw (windowPosition, windowSize), pointerPositionIncrement, screenPositionFromCellPosition, wordPosition)
import Chelleport.KeySequence (findMatchPosition, generateGrid, nextChars, toKeyChar)
import Chelleport.OCR (MonadOCR (captureScreenshot), getWordsInImage)
import Chelleport.Types
import Chelleport.Utils (cIntToInt, clamp, intToCInt, isEmpty, itemAt)
import Control.Monad (forM_)
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust)

initialState :: (Monad m) => m (State, Maybe AppAction)
initialState = do
  let cells = fromMaybe (pure undefined) $ generateGrid 0 (rows, columns) hintKeys
  pure (defaultAppState {stateGrid = cells}, Just $ SetMode defaultHintsMode)
  where
    rows = 9
    columns = 16
    hintKeys = ['A' .. 'Z']

update :: (MonadAppShell m, MonadDraw m, MonadControl m, MonadOCR m) => State -> AppAction -> m (State, Maybe AppAction)
-- Chain clicks
update state (ChainMouseClick btn) = do
  hideWindow
  let count = case stateRepetition state of 0 -> 1; n -> n
  forM_ [1 .. count] $ \_ -> do
    clickMouseButton btn
  showWindow
  pure (state {stateRepetition = 1}, Just ResetKeys)

-- HINTS MODE: Act on key inputs
update state@(State {stateMode = ModeHints}) (HandleKeyInput keycode) = do
  case (toKeyChar keycode, validNextKeys) of
    (Just keyChar, Just validChars')
      | stateIsMatched state && keyChar `elem` ("HJKL" :: String) -> do
          incr <- pointerPositionIncrement state
          let action = IncrementMouseCursor $ directionalIncrement incr keyChar
          pure (state, Just action)
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
update state@(State {stateMode = ModeSearch {searchWords, searchInputText}}) (HandleKeyInput keycode) = do
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
      | otherwise = filter (isInfixOf text . map toLower . matchText) searchWords

-- Increment highlighted index for search mode
update state (IncrementHighlightIndex n) = do
  case stateMode state of
    ModeSearch {} -> do
      action <- traverse (fmap MoveMousePosition . wordPosition) highlightedWord
      pure (state {stateRepetition = 1, stateMode = mode {searchHighlightedIndex = highlightedIndexClamped}}, action)
      where
        highlightedWord = searchFilteredWords mode `itemAt` highlightedIndex
        highlightedIndex = searchHighlightedIndex mode + n
        highlightedIndexClamped =
          if highlightedIndex < 0
            then length (searchFilteredWords mode) - 1
            else highlightedIndex `mod` length (searchFilteredWords mode)
        mode = stateMode state
    _ -> pure (state, Nothing)

-- Move mouse incrementally
update state (IncrementMouseCursor (incX, incY)) = do
  (curX, curY) <- getMousePointerPosition
  let count = case stateRepetition state of 0 -> 1; n -> n
  let pos = (cIntToInt curX + count * incX, cIntToInt curY + count * incY)
  pure (state {stateRepetition = 1}, Just $ MoveMousePosition pos)

-- Mouse button release
update state MouseDragEnd = do
  hideWindow
  releaseMouseButton
  showWindow
  pure (state {stateRepetition = 1}, Nothing)

-- Mouse button press
update state MouseDragStart = do
  hideWindow
  pressMouseButton
  showWindow
  pure (state {stateRepetition = 1}, Nothing)

-- Mouse dragging
update state MouseDragToggle
  | stateIsDragging state = pure (state {stateIsDragging = False}, Just MouseDragEnd)
  | otherwise = do pure (state {stateIsDragging = True}, Just MouseDragStart)

-- Move mouse to given position
update state (MoveMousePosition (x, y)) = do
  moveMousePointer (intToCInt x) (intToCInt y)
  pure (state, Nothing)

-- Reset entered key sequence and state
update state ResetKeys = do
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

-- Set mode
update state (SetMode mode) = do
  case mode of
    ModeHints -> pure (state {stateMode = mode}, Nothing)
    ModeSearch {} -> do
      position <- windowPosition
      size <- windowSize
      screenshot <- hideWindow >> captureScreenshot position size <* showWindow
      matches <- getWordsInImage screenshot
      let updatedMode = mode {searchWords = matches, searchFilteredWords = matches}
      pure (state {stateMode = updatedMode}, Nothing)

-- Cleanup everything and exit
update state ShutdownApp = do
  shutdownApp
  pure (state, Nothing)

-- Trigger click
update state (TriggerMouseClick btn) = do
  hideWindow
  let count = case stateRepetition state of 0 -> 1; n -> n
  forM_ [1 .. count] $ \_ -> do
    clickMouseButton btn
  pure (state {stateRepetition = 1}, Just ShutdownApp)

-- Set repetition count
update state (UpdateRepetition count) = do
  pure (state {stateRepetition = count}, Nothing)

-- Set/unset whether shift is pressed
update state (UpdateShiftState shiftPressed) =
  pure (state {stateIsShiftPressed = shiftPressed}, Nothing)
