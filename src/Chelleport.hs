module Chelleport where

import Chelleport.AppShell (Action (AppAction, SysQuit), EventHandler, Update, hideWindow, setupAppShell)
import Chelleport.Control (currentMousePosition, isKeyPress, isKeyPressWith, moveMouse, triggerMouseLeftClick)
import Chelleport.Draw (windowSize)
import Chelleport.KeySequence (eventToKeycode, findMatchPosition, generateGrid, isValidKey, nextChars, toKeyChar)
import Chelleport.Types
import Chelleport.Utils (intToCInt)
import qualified Chelleport.View
import Data.List ((\\))
import Data.Maybe (fromMaybe, isJust)
import qualified SDL

open :: IO ()
open = setupAppShell initialState update eventToAction Chelleport.View.render

initialState :: DrawContext -> IO State
initialState _ctx = do
  let cells = fromMaybe (pure undefined) $ generateGrid 0 (rows, columns) hintKeys
  pure $ State {stateGrid = cells, stateKeySequence = [], stateIsMatched = False}
  where
    rows = 12
    columns = 12
    hintKeys = ['A' .. 'Z'] \\ "Q"

update :: Update State AppAction
update state _ctx (FilterSequence key) =
  case liftA2 (,) (toKeyChar key) validChars of
    Just (keyChar, validChars')
      | stateIsMatched state && keyChar `elem` ("HJKL" :: String) -> do
          let incr = 10
          let action = IncrementMouseCursor $ incrementCursor keyChar incr
          pure (state, Just . AppAction $ action)
      | keyChar `elem` validChars' -> do
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let matchPosition = findMatchPosition newKeySequence $ stateGrid state
          let state' = state {stateKeySequence = newKeySequence, stateIsMatched = isJust matchPosition}
          pure (state', AppAction . MoveMousePosition <$> matchPosition)
    _ -> pure (state, Nothing)
  where
    validChars = nextChars (stateKeySequence state) (stateGrid state)
    incrementCursor 'H' inc = (-inc, 0)
    incrementCursor 'L' inc = (inc, 0)
    incrementCursor 'K' inc = (0, -inc)
    incrementCursor 'J' inc = (0, inc)
    incrementCursor _ _ = undefined
update state ctx (IncrementMouseCursor (incx, incy)) = do
  (SDL.V2 curx cury) <- currentMousePosition ctx
  moveMouse ctx (curx + intToCInt incx) (cury + intToCInt incy)
  pure (state, Nothing)
update state ctx (MoveMousePosition (row, col)) = do
  (x, y) <- getPosition
  moveMouse ctx x y
  pure (state, Nothing)
  where
    cellDimensions = do
      (SDL.V2 width height) <- windowSize ctx
      let rows = stateGrid state
      let wcell = width `div` intToCInt (length $ head rows)
      let hcell = height `div` intToCInt (length rows)
      pure (wcell, hcell)

    getPosition = do
      (wcell, hcell) <- cellDimensions
      let x = (wcell `div` 2) + wcell * intToCInt col
      let y = (hcell `div` 2) + hcell * intToCInt row
      pure (x, y)
update state _ctx ResetKeys = pure (state {stateKeySequence = [], stateIsMatched = False}, Nothing)
update state ctx TriggerLeftClick = do
  hideWindow ctx
  triggerMouseLeftClick ctx
  pure (state, Just SysQuit)

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
          Just $ AppAction $ FilterSequence $ eventToKeycode ev
    _ -> Nothing
