module Chelleport where

import Chelleport.AppShell (Action (AppAction, SysQuit), EventHandler, Update, hideWindow, setupAppShell)
import Chelleport.Control (currentMousePosition, eventToKeycode, isKeyPress, isKeyPressWith, isKeyReleaseWith, moveMouse, triggerMouseLeftClick)
import Chelleport.Draw (cellSize)
import Chelleport.KeySequence (findMatchPosition, generateGrid, isValidKey, nextChars, toKeyChar)
import Chelleport.Types
import Chelleport.Utils (cIntToInt, intToCInt)
import qualified Chelleport.View
import Data.List ((\\))
import Data.Maybe (fromMaybe, isJust)
import Foreign.C (CInt)
import qualified SDL

open :: IO ()
open = setupAppShell initialState update eventToAction Chelleport.View.render

initialState :: DrawContext -> IO State
initialState _ctx = do
  let cells = fromMaybe (pure undefined) $ generateGrid 0 (rows, columns) hintKeys
  pure $
    State
      { stateGrid = cells,
        stateKeySequence = [],
        stateIsMatched = False,
        stateIsShiftPressed = False
      }
  where
    rows = 12
    columns = 12
    hintKeys = ['A' .. 'Z'] \\ "Q"

directionalIncrement :: (CInt, CInt) -> Char -> (Int, Int)
directionalIncrement (incx, incy) = \case
  'H' -> (-cIntToInt incx, 0)
  'L' -> (cIntToInt incx, 0)
  'K' -> (0, -cIntToInt incy)
  'J' -> (0, cIntToInt incy)
  _ -> undefined

update :: Update State AppAction
-- Act on key inputs
update state ctx (FilterSequence key) =
  case liftA2 (,) (toKeyChar key) validChars of
    Just (keyChar, validChars')
      | stateIsMatched state && keyChar `elem` ("HJKL" :: String) -> do
          incr <- incrementValue
          let action = IncrementMouseCursor $ directionalIncrement incr keyChar
          pure (state, Just . AppAction $ action)
      | keyChar `elem` validChars' -> do
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let matchPosition = findMatchPosition newKeySequence $ stateGrid state
          let state' = state {stateKeySequence = newKeySequence, stateIsMatched = isJust matchPosition}
          pure (state', AppAction . MoveMousePosition <$> matchPosition)
    _ -> pure (state, Nothing)
  where
    validChars = nextChars (stateKeySequence state) (stateGrid state)
    incrementValue = do
      (wcell, hcell) <- cellSize state ctx
      if stateIsShiftPressed state
        then pure (wcell `div` 4, hcell `div` 4)
        else pure (wcell `div` 16, hcell `div` 16)

-- Move mouse incrementally
update state ctx (IncrementMouseCursor (incx, incy)) = do
  (SDL.V2 curx cury) <- currentMousePosition ctx
  moveMouse ctx (curx + intToCInt incx) (cury + intToCInt incy)
  pure (state, Nothing)

-- Move mouse to given position
update state ctx (MoveMousePosition (row, col)) = do
  (x, y) <- getPosition
  moveMouse ctx x y
  pure (state, Nothing)
  where
    getPosition = do
      (wcell, hcell) <- cellSize state ctx
      let x = (wcell `div` 2) + wcell * intToCInt col
      let y = (hcell `div` 2) + hcell * intToCInt row
      pure (x, y)

-- Reset entered key sequence and state
update state _ctx ResetKeys = do
  pure (state {stateKeySequence = [], stateIsMatched = False}, Nothing)

-- Trigger left click
update state ctx TriggerLeftClick = do
  hideWindow ctx
  triggerMouseLeftClick ctx
  pure (state, Just SysQuit)

-- Set/unset whether shift is pressed
update state _ctx (UpdateShiftState shift) = pure (state {stateIsShiftPressed = shift}, Nothing)

eventToAction :: EventHandler State AppAction
eventToAction _state event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just SysQuit
    SDL.KeyboardEvent ev
      | isKeyPressWith ev SDL.KeycodeQ -> Just SysQuit
      | isKeyPressWith ev SDL.KeycodeEscape -> Just SysQuit
      | isKeyPressWith ev SDL.KeycodeSpace -> Just $ AppAction TriggerLeftClick
      | isKeyPressWith ev SDL.KeycodeTab -> Just $ AppAction ResetKeys
      | isKeyPress ev && isValidKey (eventToKeycode ev) ->
          Just . AppAction $ FilterSequence $ eventToKeycode ev
      | isKeyPressWith ev SDL.KeycodeLShift || isKeyPressWith ev SDL.KeycodeRShift ->
          Just . AppAction $ UpdateShiftState True
      | isKeyReleaseWith ev SDL.KeycodeLShift || isKeyReleaseWith ev SDL.KeycodeRShift ->
          Just . AppAction $ UpdateShiftState False
    _ -> Nothing
