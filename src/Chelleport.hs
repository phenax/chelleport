module Chelleport where

import Chelleport.AppShell (Action (AppAction, SysQuit), DrawContext, setupAppShell)
import Chelleport.Draw (renderText)
import qualified Data.Text as Text
import qualified SDL

newtype State = State
  { stateCount :: Int
  }

newtype AppAction = ActionUpdateCount Int

open :: IO ()
open = setupAppShell initialState update eventToAction render

initialState :: State
initialState = State {stateCount = 0}

render :: State -> DrawContext -> IO ()
render state ctx = do
  renderText ctx $ Text.pack $ "Hello" ++ show (stateCount state)

update :: State -> DrawContext -> AppAction -> IO State
update state _ctx (ActionUpdateCount count) = do
  -- SDL.warpMouse SDL.WarpGlobal $ SDL.P $ SDL.V2 (unsafeCoerce $ 10 * stateCount state) 100
  pure state {stateCount = count}

eventToAction :: State -> SDL.Event -> Maybe (Action AppAction)
eventToAction state event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent ev
      | isKeyPress ev SDL.KeycodeQ -> Just SysQuit
      | isKeyPress ev SDL.KeycodeEscape -> Just SysQuit
      | isKeyPress ev SDL.KeycodeJ -> Just $ AppAction $ ActionUpdateCount (stateCount state - 1)
      | isKeyPress ev SDL.KeycodeK -> Just $ AppAction $ ActionUpdateCount (stateCount state + 1)
    _ -> Nothing

isKeyPress :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPress keyboardEvent keyCode =
  SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
    && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode
