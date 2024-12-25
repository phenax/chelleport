module Chelleport where

import Chelleport.AppShell (setupAppShell)
import qualified Chelleport.AppState as AppState
import Chelleport.Context (initializeContext)
import Chelleport.Control (anyAlphanumeric, anyDigit, checkKey, ctrl, eventToKeycode, hjklDirection, key, pressed, released, shift)
import Chelleport.KeySequence (keycodeToInt, toKeyChar)
import Chelleport.Types
import Chelleport.Utils ((<||>))
import qualified Chelleport.View
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Maybe (fromMaybe)
import qualified SDL

run :: IO ()
run = do
  ctx <- initializeContext
  runAppWithCtx ctx $
    setupAppShell
      ctx
      AppState.initialState
      AppState.update
      eventHandler
      Chelleport.View.render
  where
    runAppWithCtx :: (MonadIO m) => DrawContext -> AppM m x -> m x
    runAppWithCtx ctx = (`runReaderT` ctx) . runAppM

eventHandler :: State -> SDL.Event -> Maybe AppAction
eventHandler state event = do
  let hjkl = key SDL.KeycodeH <||> key SDL.KeycodeJ <||> key SDL.KeycodeK <||> key SDL.KeycodeL
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just ShutdownApp
    SDL.KeyboardEvent ev
      -- Esc: Quit
      | checkKey [key SDL.KeycodeEscape, pressed] ev -> Just ShutdownApp
      -- <C-s>: Enable search mode
      | checkKey [ctrl, key SDL.KeycodeS, pressed] ev -> Just $ SetMode defaultSearchMode
      -- <C-t>: Enable hints mode
      | checkKey [ctrl, key SDL.KeycodeT, pressed] ev -> Just $ SetMode defaultHintsMode
      -- <C-n>, <C-p>: Search increment next/prev
      | checkKey [ctrl, key SDL.KeycodeN, pressed] ev -> Just $ IncrementHighlightIndex (stateRepetition state)
      | checkKey [ctrl, key SDL.KeycodeP, pressed] ev -> Just $ IncrementHighlightIndex (-1 * stateRepetition state)
      -- <C-hjkl>: Movement
      | checkKey [ctrl, hjkl, pressed] ev ->
          MoveMouseInDirection . hjklDirection <$> toKeyChar (eventToKeycode ev)
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
