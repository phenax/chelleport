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

currentMousePosition :: DrawContext -> IO (SDL.V2 CInt)
currentMousePosition _ctx = do
  (SDL.P p) <- SDL.getAbsoluteMouseLocation
  pure p

isKeyPress :: SDL.KeyboardEventData -> Bool
isKeyPress = (== SDL.Pressed) . SDL.keyboardEventKeyMotion

isKeyRelease :: SDL.KeyboardEventData -> Bool
isKeyRelease = (== SDL.Released) . SDL.keyboardEventKeyMotion

eventToKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventToKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

isKeyPressWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPressWith keyboardEvent keyCode =
  isKeyPress keyboardEvent && eventToKeycode keyboardEvent == keyCode

isKeyReleaseWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyReleaseWith keyboardEvent keyCode =
  isKeyRelease keyboardEvent && eventToKeycode keyboardEvent == keyCode
