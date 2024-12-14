module Chelleport.Control where

import Chelleport.Context (DrawContext (ctxX11Display))
import Foreign.C (CInt)
import qualified Graphics.X11 as X11
import qualified Graphics.X11.XTest as X11
import qualified Graphics.X11.Xlib.Extras as X11
import qualified SDL

triggerMouseLeftClick :: DrawContext -> IO ()
triggerMouseLeftClick ctx = do
  let display = ctxX11Display ctx
  X11.fakeButtonPress display X11.button1
  X11.sync display False

moveMouse :: CInt -> CInt -> IO ()
moveMouse x y = do
  SDL.warpMouse SDL.WarpGlobal (SDL.P $ SDL.V2 x y)
