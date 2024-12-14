module Chelleport.Context where

import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

data DrawContext = DrawContext
  { ctxWindow :: SDL.Window,
    ctxRenderer :: SDL.Renderer,
    ctxFont :: TTF.Font,
    ctxX11Display :: X11.Display
  }

createContext :: IO DrawContext
createContext = do
  -- bounds <- fmap SDL.displayBoundsSize <$> SDL.getDisplays
  -- let windowSize = case bounds of
  --       (x : _) -> x
  --       _ -> SDL.V2 800 600
  let windowSize = SDL.V2 0 0

  let windowCfg =
        SDL.defaultWindow
          { SDL.windowInputGrabbed = True,
            SDL.windowMode = SDL.FullscreenDesktop,
            SDL.windowPosition = SDL.Absolute $ SDL.P $ SDL.V2 0 0,
            SDL.windowInitialSize = windowSize,
            SDL.windowBorder = False
          }
  window <- SDL.createWindow "My SDL Application" windowCfg
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- TTF.load "Inter-Regular.ttf" 16

  SDL.windowOpacity window $= 0.6
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend

  display <- X11.openDisplay ""

  pure $
    DrawContext
      { ctxWindow = window,
        ctxRenderer = renderer,
        ctxFont = font,
        ctxX11Display = display
      }
