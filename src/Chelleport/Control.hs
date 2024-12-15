module Chelleport.Control where

import Chelleport.Types
import Control.Concurrent (threadDelay)
import Foreign.C (CInt)
import qualified Graphics.X11 as X11
import qualified Graphics.X11.XTest as X11
import qualified SDL

triggerMouseLeftClick :: DrawContext -> IO ()
triggerMouseLeftClick (DrawContext {ctxX11Display = display}) = do
  threadDelay 30_000 -- Wrap with delay to prevent async window close issues. TODO: Remove maybe?
  X11.fakeButtonPress display X11.button1
  X11.sync display False
  threadDelay 30_000

moveMouse :: DrawContext -> CInt -> CInt -> IO ()
moveMouse _ x y = do
  SDL.warpMouse SDL.WarpGlobal (SDL.P $ SDL.V2 x y)

isKeyPress :: SDL.KeyboardEventData -> Bool
isKeyPress = (== SDL.Pressed) . SDL.keyboardEventKeyMotion

isKeyPressWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPressWith keyboardEvent keyCode =
  isKeyPress keyboardEvent && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode
