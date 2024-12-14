module Chelleport.Context where

import Chelleport.Types
import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

initializeContext :: IO DrawContext
initializeContext = do
  let windowCfg =
        SDL.defaultWindow
          { SDL.windowMode = SDL.FullscreenDesktop,
            SDL.windowPosition = SDL.Absolute $ SDL.P $ SDL.V2 0 0,
            SDL.windowInitialSize = SDL.V2 0 0,
            SDL.windowBorder = False
          }
  window <- SDL.createWindow "Chelleport" windowCfg
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- TTF.load "Inter-Regular.ttf" 24

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
