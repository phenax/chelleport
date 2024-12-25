module Chelleport where

import Chelleport.AppShell (MonadAppShell (hideWindow, showWindow, shutdownApp), setupAppShell)
import Chelleport.Context (initializeContext)
import Chelleport.Control
  ( MonadControl (clickMouseButton, getMousePointerPosition, moveMousePointer, pressMouseButton, releaseMouseButton),
    anyAlphanumeric,
    anyDigit,
    checkKey,
    ctrl,
    directionalIncrement,
    eventToKeycode,
    key,
    pressed,
    released,
    shift,
  )
import Chelleport.Draw (MonadDraw (windowPosition, windowSize), cellSize)
import Chelleport.KeySequence (findMatchPosition, generateGrid, keycodeToInt, nextChars, toKeyChar)
import Chelleport.OCR (MonadOCR (captureScreenshot), getWordsInImage)
import Chelleport.Types
import Chelleport.Utils (cIntToInt, clamp, intToCInt, isEmpty, itemAt, (<||>))
import qualified Chelleport.View
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe, isJust)
import qualified SDL

run :: IO ()
run = do
  ctx <- initializeContext
  runAppWithCtx ctx $
    setupAppShell
      ctx
      initialState
      update
      eventHandler
      Chelleport.View.render
  where
    runAppWithCtx :: (MonadIO m) => DrawContext -> AppM m x -> m x
    runAppWithCtx ctx = (`runReaderT` ctx) . runAppM

initialState :: (Monad m) => m (State, Maybe AppAction)
initialState = do
  let cells = fromMaybe (pure undefined) $ generateGrid 0 (rows, columns) hintKeys
  pure (defaultAppState {stateGrid = cells}, Just $ SetMode defaultHintsMode)
  where
    rows = 9
    columns = 16
    hintKeys = ['A' .. 'Z']

eventHandler :: State -> SDL.Event -> Maybe AppAction
eventHandler state event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just ShutdownApp
    SDL.KeyboardEvent ev
      -- Esc: Quit
      | checkKey [key SDL.KeycodeEscape, pressed] ev -> Just ShutdownApp
      -- <C-s>: Enable search mode
      | checkKey [ctrl, key SDL.KeycodeS, pressed] ev -> Just $ SetMode defaultSearchMode
      -- <C-h>: Enable hints mode
      | checkKey [ctrl, key SDL.KeycodeH, pressed] ev -> Just $ SetMode defaultHintsMode
      -- <C-n>, <C-p>: Search increment next/prev
      | checkKey [ctrl, key SDL.KeycodeN, pressed] ev -> Just $ IncrementHighlightIndex (stateRepetition state)
      | checkKey [ctrl, key SDL.KeycodeP, pressed] ev -> Just $ IncrementHighlightIndex (-1 * stateRepetition state)
      -- Space / Shift+Space : Left click/chain left click
      | checkKey [key SDL.KeycodeSpace, pressed] ev ->
          if shift ev
            then Just $ ChainMouseClick LeftClick
            else Just $ TriggerMouseClick LeftClick
      -- Backspace: Reset keys
      | checkKey [key SDL.KeycodeBackspace, pressed] ev -> Just ResetKeys
      -- <C-v>: Toggle mouse dragging
      | checkKey [ctrl, key SDL.KeycodeV, pressed] ev -> Just MouseDragToggle
      -- minus / underscore: Right click/chain right click
      | checkKey [key SDL.KeycodeMinus <||> key SDL.KeycodeUnderscore, pressed] ev ->
          if shift ev
            then Just $ ChainMouseClick RightClick
            else Just $ TriggerMouseClick RightClick
      -- 0-9: Repetition digit
      | checkKey [anyDigit, pressed] ev ->
          Just $ UpdateRepetition (fromMaybe 0 $ keycodeToInt $ eventToKeycode ev)
      -- A-Z
      | checkKey [anyAlphanumeric, pressed] ev -> Just $ HandleKeyInput $ eventToKeycode ev
      -- Shift press/release: Toggle shift mode
      | checkKey [pressed, key SDL.KeycodeRShift <||> key SDL.KeycodeLShift] ev -> Just $ UpdateShiftState True
      | checkKey [released, key SDL.KeycodeRShift <||> key SDL.KeycodeLShift] ev -> Just $ UpdateShiftState False
    _ -> Nothing

wordPosition :: (MonadDraw m) => OCRMatch -> m (Int, Int)
wordPosition (OCRMatch {matchStartX, matchStartY}) = do
  (x, y) <- windowPosition
  pure (cIntToInt $ x + matchStartX, cIntToInt $ y + matchStartY)

update :: (MonadAppShell m, MonadDraw m, MonadControl m, MonadOCR m) => State -> AppAction -> m (State, Maybe AppAction)
-- Set mode
update state (SetMode mode) = do
  case mode of
    ModeHints -> pure (state {stateMode = mode}, Nothing)
    ModeSearch {} -> do
      pos <- windowPosition
      size <- windowSize
      screenshot <- hideWindow >> captureScreenshot pos size <* showWindow

      wordsOnScreen <- getWordsInImage screenshot
      let updatedMode = mode {searchWords = wordsOnScreen, searchFilteredWords = wordsOnScreen}
      pure (state {stateMode = updatedMode}, Nothing)

-- HINTS MODE: Act on key inputs
update state@(State {stateMode = ModeHints}) (HandleKeyInput keycode) = do
  case (toKeyChar keycode, validChars) of
    (Just keyChar, Just validChars')
      | stateIsMatched state && keyChar `elem` ("HJKL" :: String) -> do
          incr <- incrementValue
          let action = IncrementMouseCursor $ directionalIncrement incr keyChar
          pure (state, Just action)
      | keyChar `elem` validChars' -> do
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let matchPosition = findMatchPosition newKeySequence $ stateGrid state
          let state' = state {stateKeySequence = newKeySequence, stateIsMatched = isJust matchPosition}
          action <- maybe (pure Nothing) (fmap (Just . MoveMousePosition) . getPosition) matchPosition
          pure (state', action)
    _ -> pure (state, Nothing)
  where
    validChars = nextChars (stateKeySequence state) (stateGrid state)
    getPosition (row, col) = do
      (wcell, hcell) <- cellSize state
      let x = (wcell `div` 2) + wcell * intToCInt col
      let y = (hcell `div` 2) + hcell * intToCInt row
      (winx, winy) <- windowPosition
      pure (cIntToInt $ winx + x, cIntToInt $ winy + y)
    incrementValue = do
      (wcell, hcell) <- cellSize state
      if stateIsShiftPressed state
        then pure (wcell `div` 4, hcell `div` 4)
        else pure (wcell `div` 16, hcell `div` 16)

-- SEARCH MODE: Act on key inputs
update state@(State {stateMode = ModeSearch {searchWords, searchInputText}}) (HandleKeyInput keycode) = do
  case toKeyChar keycode of
    Just keyChar -> do
      let searchText = searchInputText ++ [toLower keyChar]
      let matches = filterMatches searchText
      let mode = stateMode state
      let highlightedIndex = clamp (0, length matches - 1) (searchHighlightedIndex mode)
      let updatedMode =
            mode
              { searchInputText = searchText,
                searchFilteredWords = matches,
                searchHighlightedIndex = highlightedIndex
              }
      let highlightedWord = matches `itemAt` highlightedIndex
      action <- maybe (pure Nothing) (fmap (Just . MoveMousePosition) . wordPosition) highlightedWord
      pure (state {stateMode = updatedMode}, action)
    _ -> do
      pure (state, Nothing)
  where
    filterMatches text
      | isEmpty text = searchWords
      | otherwise = filter (isInfixOf text . map toLower . matchText) searchWords

-- Increment highlighted index for search mode
update state (IncrementHighlightIndex n) = do
  case stateMode state of
    ModeSearch {} -> do
      let mode = stateMode state
      let index = searchHighlightedIndex mode + n
      let highlightedIndex =
            if index < 0
              then length (searchFilteredWords mode) - 1
              else index `mod` length (searchFilteredWords mode)
      let highlightedWord = searchFilteredWords mode `itemAt` highlightedIndex
      action <- maybe (pure Nothing) (fmap (Just . MoveMousePosition) . wordPosition) highlightedWord
      pure (state {stateRepetition = 1, stateMode = mode {searchHighlightedIndex = highlightedIndex}}, action)
    _ -> pure (state, Nothing)

-- Move mouse incrementally
update state (IncrementMouseCursor (incX, incY)) = do
  (curX, curY) <- getMousePointerPosition
  let count = case stateRepetition state of 0 -> 1; n -> n
  let pos = (cIntToInt curX + count * incX, cIntToInt curY + count * incY)
  pure (state {stateRepetition = 1}, Just $ MoveMousePosition pos)

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

-- Trigger click
update state (TriggerMouseClick btn) = do
  hideWindow
  let count = case stateRepetition state of 0 -> 1; n -> n
  forM_ [1 .. count] $ \_ -> do
    clickMouseButton btn
  pure (state {stateRepetition = 1}, Just ShutdownApp)

-- Chain clicks
update state (ChainMouseClick btn) = do
  hideWindow
  let count = case stateRepetition state of 0 -> 1; n -> n
  forM_ [1 .. count] $ \_ -> do
    clickMouseButton btn
  showWindow
  pure (state {stateRepetition = 1}, Just ResetKeys)

-- Cleanup everything and exit
update state ShutdownApp = do
  shutdownApp
  pure (state, Nothing)

-- Mouse dragging
update state MouseDragToggle
  | stateIsDragging state = pure (state {stateIsDragging = False}, Just MouseDragEnd)
  | otherwise = do pure (state {stateIsDragging = True}, Just MouseDragStart)

-- Mouse button press
update state MouseDragStart = do
  hideWindow
  pressMouseButton
  showWindow
  pure (state {stateRepetition = 1}, Nothing)

-- Mouse button release
update state MouseDragEnd = do
  hideWindow
  releaseMouseButton
  showWindow
  pure (state {stateRepetition = 1}, Nothing)

-- Set repetition count
update state (UpdateRepetition count) = do
  pure (state {stateRepetition = count}, Nothing)

-- Set/unset whether shift is pressed
update state (UpdateShiftState shiftPressed) =
  pure (state {stateIsShiftPressed = shiftPressed}, Nothing)
