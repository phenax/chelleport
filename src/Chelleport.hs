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
update state _ctx SetupGrid = pure state
update state ctx TriggerLeftClick = do
  hideWindow ctx
  triggerMouseLeftClick ctx
  shutdownApp ctx
  pure state
update state ctx (FilterSequence key) =
  case validChars >>= (\chars -> (,chars) <$> toKeyChar key) of
    Just (keyChar, validChars')
      | keyChar `elem` validChars' -> do
          (SDL.V2 width height) <- SDL.get $ SDL.windowSize $ ctxWindow ctx
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let rows = stateCells state
          let wcell = width `div` unsafeCoerce (length $ head rows)
          let hcell = height `div` unsafeCoerce (length rows)
          case findMatchPosition newKeySequence rows of
            Just (row, col) -> do
              moveMouse ctx (wcell * unsafeCoerce col) (hcell * unsafeCoerce row)
            Nothing -> pure ()
          pure state {stateKeySequence = newKeySequence}
    _ -> pure state
  where
    validChars = nextChars (stateKeySequence state) (stateCells state)

eventToAction :: State -> SDL.Event -> Maybe (Action AppAction)
eventToAction _state event =
  case SDL.eventPayload event of
    -- SDL.WindowShownEvent _ -> Just $ AppAction SetupGrid
    SDL.QuitEvent -> Just SysQuit
    SDL.KeyboardEvent ev
      | isKeyPressWith ev SDL.KeycodeQ -> Just SysQuit
      | isKeyPressWith ev SDL.KeycodeEscape -> Just SysQuit
      | isKeyPressWith ev SDL.KeycodeSpace -> Just $ AppAction TriggerLeftClick
      | isKeyPress ev && isValidKey (eventToKeycode ev) ->
          Just $ AppAction $ FilterSequence $ eventToKeycode ev
    _ -> Nothing
