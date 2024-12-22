module Chelleport where

import Chelleport.AppShell (MonadAppShell (hideWindow, showWindow, shutdownApp), setupAppShell)
import Chelleport.Context (initializeContext)
import Chelleport.Control
  ( MonadControl (clickMouseButton, getMousePointerPosition, moveMousePointer, pressMouseButton, releaseMouseButton),
    directionalIncrement,
    eventToKeycode,
    isKeyPressWith,
    isKeyPressed,
    isKeyReleaseWith,
    withCtrl,
    withShift,
  )
import Chelleport.Draw (MonadDraw (windowPosition), cellSize)
import Chelleport.KeySequence (findMatchPosition, generateGrid, isKeycodeDigit, isValidKey, keycodeToInt, nextChars, toKeyChar)
import Chelleport.Types
import Chelleport.Utils (intToCInt)
import qualified Chelleport.View
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
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
      (const eventHandler)
      Chelleport.View.render
  where
    runAppWithCtx :: (MonadIO m) => DrawContext -> AppM m x -> m x
    runAppWithCtx ctx = (`runReaderT` ctx) . runAppM

initialState :: (Monad m) => m State
initialState = do
  let cells = fromMaybe (pure undefined) $ generateGrid 0 (rows, columns) hintKeys
  pure $
    State
      { stateGrid = cells,
        stateKeySequence = [],
        stateIsMatched = False,
        stateIsShiftPressed = False,
        stateIsDragging = False,
        stateRepetition = 1
      }
  where
    rows = 9
    columns = 16
    hintKeys = ['A' .. 'Z']

eventHandler :: SDL.Event -> Maybe AppAction
eventHandler event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just ShutdownApp
    SDL.KeyboardEvent ev
      -- Escape
      | isKeyPressWith ev SDL.KeycodeEscape ->
          Just ShutdownApp
      -- minus / underscore
      | isKeyPressWith ev SDL.KeycodeMinus || isKeyPressWith ev SDL.KeycodeUnderscore ->
          if withShift ev
            then Just $ ChainMouseClick RightClick
            else Just $ TriggerMouseClick RightClick
      -- 0-9
      | isKeycodeDigit (eventToKeycode ev) ->
          Just $ UpdateRepetition (fromMaybe 0 $ keycodeToInt $ eventToKeycode ev)
      -- Space / Shift+Space
      | isKeyPressWith ev SDL.KeycodeSpace ->
          if withShift ev
            then Just $ ChainMouseClick LeftClick
            else Just $ TriggerMouseClick LeftClick
      -- Tab / Backspace
      | isKeyPressWith ev SDL.KeycodeTab || isKeyPressWith ev SDL.KeycodeBackspace ->
          Just ResetKeys
      -- Ctrl + V
      | withCtrl ev && isKeyPressWith ev SDL.KeycodeV ->
          Just MouseDragToggle
      -- A-Z
      | isKeyPressed ev && isValidKey (eventToKeycode ev) ->
          Just $ HandleKeyInput $ eventToKeycode ev
      -- Shift press
      | isKeyPressWith ev SDL.KeycodeLShift || isKeyPressWith ev SDL.KeycodeRShift ->
          Just $ UpdateShiftState True
      -- Shift release
      | isKeyReleaseWith ev SDL.KeycodeLShift || isKeyReleaseWith ev SDL.KeycodeRShift ->
          Just $ UpdateShiftState False
    _ -> Nothing

update :: (MonadAppShell m, MonadDraw m, MonadControl m) => State -> AppAction -> m (State, Maybe AppAction)
-- Act on key inputs
update state (HandleKeyInput key) = do
  case (toKeyChar key, validChars) of
    (Just keyChar, Just validChars')
      | stateIsMatched state && keyChar `elem` ("HJKL" :: String) -> do
          incr <- incrementValue
          let action = IncrementMouseCursor $ directionalIncrement incr keyChar
          pure (state, Just action)
      | keyChar `elem` validChars' -> do
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let matchPosition = findMatchPosition newKeySequence $ stateGrid state
          let state' = state {stateKeySequence = newKeySequence, stateIsMatched = isJust matchPosition}
          pure (state', MoveMousePosition <$> matchPosition)
    _ -> pure (state, Nothing)
  where
    validChars = nextChars (stateKeySequence state) (stateGrid state)
    incrementValue = do
      (wcell, hcell) <- cellSize state
      if stateIsShiftPressed state
        then pure (wcell `div` 4, hcell `div` 4)
        else pure (wcell `div` 16, hcell `div` 16)

-- Move mouse incrementally
update state (IncrementMouseCursor (incX, incY)) = do
  (curX, curY) <- getMousePointerPosition
  let count = intToCInt $ case stateRepetition state of 0 -> 1; n -> n
  moveMousePointer (curX + count * intToCInt incX) (curY + count * intToCInt incY)
  pure (state {stateRepetition = 1}, Nothing)

-- Move mouse to given position
update state (MoveMousePosition (row, col)) = do
  (x, y) <- getPosition
  moveMousePointer x y
  pure (state, Nothing)
  where
    getPosition = do
      (wcell, hcell) <- cellSize state
      let x = (wcell `div` 2) + wcell * intToCInt col
      let y = (hcell `div` 2) + hcell * intToCInt row
      (winx, winy) <- windowPosition
      pure (winx + x, winy + y)

-- Reset entered key sequence and state
update state ResetKeys = do
  pure (state {stateKeySequence = [], stateIsMatched = False, stateRepetition = 1}, Nothing)

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
update state (UpdateShiftState shift) =
  pure (state {stateIsShiftPressed = shift}, Nothing)
