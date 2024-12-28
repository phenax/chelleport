module Chelleport where

import Chelleport.AppShell (setupAppShell)
import qualified Chelleport.AppState as AppState
import Chelleport.Context (initializeContext)
import Chelleport.Control (anyAlphabetic, anyDigit, checkKey, ctrl, eventToKeycode, hjkl, hjklDirection, key, pressed, released, shift)
import Chelleport.KeySequence (keycodeToInt, toKeyChar)
import Chelleport.Types
import Chelleport.Utils ((<||>))
import qualified Chelleport.View as View
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Default (Default (def))
import Data.Maybe (fromMaybe)
import qualified SDL

run :: Configuration -> IO ()
run config = do
  ctx <- initializeContext
  -- Cosplaying as elm
  runAppWithCtx ctx $
    setupAppShell ctx (AppState.initialState config) AppState.update eventHandler View.render
  where
    runAppWithCtx :: (MonadIO m) => DrawContext -> AppM m x -> m x
    runAppWithCtx ctx = (`runReaderT` ctx) . runAppM

eventHandler :: State -> SDL.Event -> Maybe AppAction
eventHandler state event =
  case SDL.eventPayload event of
    SDL.QuitEvent -> Just ShutdownApp
    SDL.KeyboardEvent ev
      -- Esc: Quit
      | checkKey [key SDL.KeycodeEscape, pressed] ev ->
          Just ShutdownApp
      -- <C-s>: Enable search mode
      | checkKey [ctrl, key SDL.KeycodeS, pressed] ev ->
          Just $ SetMode $ ModeSearch def
      -- <C-t>: Enable hints mode
      | checkKey [ctrl, key SDL.KeycodeT, pressed] ev ->
          Just $ SetMode $ ModeHints def
      -- <C-n>, <C-p>: Search increment next/prev
      | checkKey [ctrl, key SDL.KeycodeN, pressed] ev ->
          Just $ IncrementHighlightIndex (stateRepetition state)
      | checkKey [ctrl, key SDL.KeycodeP, pressed] ev ->
          Just $ IncrementHighlightIndex (-1 * stateRepetition state)
      -- <C-hjkl>: Movement
      | checkKey [ctrl, hjkl, pressed] ev ->
          MoveMouseInDirection . hjklDirection <$> toKeyChar (eventToKeycode ev)
      -- Space / Enter / Shift+Space / Shift+Enter : Left click/chain left click
      | checkKey [key SDL.KeycodeSpace <||> key SDL.KeycodeReturn, pressed] ev ->
          if shift ev
            then Just $ ChainMouseClick LeftClick
            else Just $ TriggerMouseClick LeftClick
      -- Backspace: Reset keys
      | checkKey [key SDL.KeycodeBackspace, pressed] ev ->
          Just ResetKeys
      -- <C-v>: Toggle mouse dragging
      | checkKey [ctrl, key SDL.KeycodeV, pressed] ev ->
          Just MouseDragToggle
      -- minus / underscore: Right click/chain right click
      | checkKey [key SDL.KeycodeMinus <||> key SDL.KeycodeUnderscore, pressed] ev ->
          if shift ev
            then Just $ ChainMouseClick RightClick
            else Just $ TriggerMouseClick RightClick
      -- 0-9: Repetition digit
      | checkKey [anyDigit, not . ctrl, pressed] ev ->
          Just $ UpdateRepetition (fromMaybe 0 $ keycodeToInt $ eventToKeycode ev)
      -- A-Z: hint keys and search text
      | checkKey [anyAlphabetic, not . ctrl, pressed] ev ->
          Just $ HandleKeyInput $ eventToKeycode ev
      -- Shift press/release: Toggle shift mode
      | checkKey [pressed, key SDL.KeycodeRShift <||> key SDL.KeycodeLShift] ev ->
          Just $ UpdateShiftState True
      | checkKey [released, key SDL.KeycodeRShift <||> key SDL.KeycodeLShift] ev ->
          Just $ UpdateShiftState False
    _ -> Nothing
