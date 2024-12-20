{-# LANGUAGE MultiParamTypeClasses #-}

module Chelleport where

import Chelleport.AppShell (MonadAppShell (hideWindow, shutdownApp), setupAppShell)
import Chelleport.Context (initializeContext)
import Chelleport.Control (MonadControl (getMousePointerPosition, moveMousePointer, pressMouseButton), directionalIncrement, eventToKeycode, isKeyPress, isKeyPressWith, isKeyReleaseWith)
import Chelleport.Draw (MonadDraw, cellSize)
import Chelleport.KeySequence (findMatchPosition, generateGrid, isValidKey, nextChars, toKeyChar)
import Chelleport.Types
import Chelleport.Utils (intToCInt)
import qualified Chelleport.View
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.List ((\\))
import Data.Maybe (fromMaybe, isJust)
import qualified SDL

runEff ::
  (MonadIO m) =>
  DrawContext ->
  AppM m x ->
  m x
runEff ctx action = runReaderT (runAppM action) ctx

run :: IO ()
run = do
  ctx <- initializeContext
  setupAppShell
    ctx
    (runEff ctx initialState)
    (\state -> runEff ctx . update state)
    (const eventHandler)
    (runEff ctx . Chelleport.View.render)

initialState :: (MonadIO m) => m State
initialState = do
  let cells = fromMaybe (pure undefined) $ generateGrid 0 (rows, columns) hintKeys
  pure $
    State
      { stateGrid = cells,
        stateKeySequence = [],
        stateIsMatched = False,
        stateIsShiftPressed = False
      }
  where
    rows = 9
    columns = 16
    hintKeys = ['A' .. 'Z'] \\ "Q"

eventHandler :: SDL.Event -> Maybe AppAction
eventHandler event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just ShutdownApp
    SDL.KeyboardEvent ev
      | isKeyPressWith ev SDL.KeycodeQ -> Just ShutdownApp
      | isKeyPressWith ev SDL.KeycodeEscape -> Just ShutdownApp
      | isKeyPressWith ev SDL.KeycodeSpace -> Just TriggerLeftClick
      | isKeyPressWith ev SDL.KeycodeTab -> Just ResetKeys
      | isKeyPress ev && isValidKey (eventToKeycode ev) ->
          Just $ HandleKeyInput $ eventToKeycode ev
      | isKeyPressWith ev SDL.KeycodeLShift || isKeyPressWith ev SDL.KeycodeRShift ->
          Just $ UpdateShiftState True
      | isKeyReleaseWith ev SDL.KeycodeLShift || isKeyReleaseWith ev SDL.KeycodeRShift ->
          Just $ UpdateShiftState False
    _ -> Nothing

update ::
  (MonadAppShell m, MonadDraw m, MonadControl m) =>
  State ->
  AppAction ->
  m (State, Maybe AppAction)
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
  moveMousePointer (curX + intToCInt incX) (curY + intToCInt incY)
  pure (state, Nothing)

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
      pure (x, y)

-- Reset entered key sequence and state
update state ResetKeys = do
  pure (state {stateKeySequence = [], stateIsMatched = False}, Nothing)

-- Trigger left click
update state TriggerLeftClick = do
  hideWindow
  pressMouseButton LeftClick
  pure (state, Just ShutdownApp)

-- Cleanup everything and exit
update state ShutdownApp = do
  shutdownApp
  pure (state, Nothing)

-- Set/unset whether shift is pressed
update state (UpdateShiftState shift) =
  pure (state {stateIsShiftPressed = shift}, Nothing)
