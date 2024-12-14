module Chelleport where

import Chelleport.AppShell (Action (AppAction, SysQuit), hideWindow, setupAppShell, shutdownApp)
import Chelleport.Control (isKeyPress, isKeyPressWith, moveMouse, triggerMouseLeftClick)
import Chelleport.KeySequence (eventToKeycode, findMatchPosition, generateKeyCells, isValidKey, nextChars, toKeyChar)
import Chelleport.Types
import qualified Chelleport.View
import qualified SDL
import Unsafe.Coerce (unsafeCoerce)

open :: IO ()
open = setupAppShell initialState update eventToAction Chelleport.View.render

initialState :: DrawContext -> IO State
initialState _ctx = do
  let cells = generateKeyCells (rows, columns) hintKeys
  pure $ State {stateCells = cells, stateKeySequence = []}
  where
    rows = 16
    columns = 16
    hintKeys = "ABCDEFGHIJKLMNOPRSTUVWXYZ1234567890"

update :: State -> DrawContext -> AppAction -> IO State
update state _ctx ResetKeys = pure state {stateKeySequence = []}
update state ctx TriggerLeftClick = do
  hideWindow ctx
  triggerMouseLeftClick ctx
  shutdownApp ctx
  pure state
update state ctx (FilterSequence key) =
  case liftA2 (,) (toKeyChar key) validChars of
    Just (keyChar, validChars')
      | keyChar `elem` validChars' -> do
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let matchPosition = findMatchPosition newKeySequence $ stateCells state
          maybe (pure ()) moveMouseToCell matchPosition
          pure state {stateKeySequence = newKeySequence}
    _ -> pure state
  where
    validChars = nextChars (stateKeySequence state) (stateCells state)

    cellDimensions = do
      (SDL.V2 width height) <- SDL.get $ SDL.windowSize $ ctxWindow ctx
      let rows = stateCells state
      let wcell = width `div` unsafeCoerce (length $ head rows)
      let hcell = height `div` unsafeCoerce (length rows)
      pure (wcell, hcell)

    moveMouseToCell (row, col) = do
      (wcell, hcell) <- cellDimensions
      let x = (wcell `div` 2) + wcell * unsafeCoerce col
      let y = (hcell `div` 2) + hcell * unsafeCoerce row
      moveMouse ctx x y

eventToAction :: State -> SDL.Event -> Maybe (Action AppAction)
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
