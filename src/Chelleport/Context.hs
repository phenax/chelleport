module Chelleport.Context (initializeContext) where

import Chelleport.Types
import Data.ByteString (ByteString)
import Data.FileEmbed (embedFileRelative)
import Foreign.C (CFloat)
import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

windowOpacity :: CFloat
windowOpacity = 0.5

fontSize :: Int
fontSize = 24

initializeContext :: IO DrawContext
initializeContext = do
  window <- initializeWindow
  renderer <- initializeRenderer window
  font <- loadFont

  display <- X11.openDisplay ""

  pure $
    DrawContext
      { ctxWindow = window,
        ctxRenderer = renderer,
        ctxFont = font,
        ctxX11Display = display
      }

rawFontData :: ByteString
rawFontData = $(embedFileRelative "./static/font.ttf")

loadFont :: IO TTF.Font
loadFont = do
  font <- TTF.decode rawFontData fontSize
  TTF.setStyle font [TTF.Bold]
  pure font

initializeRenderer :: SDL.Window -> IO SDL.Renderer
initializeRenderer window = do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  SDL.windowOpacity window $= windowOpacity
  SDL.rendererDrawBlendMode renderer $= SDL.BlendAlphaBlend
  pure renderer

initializeWindow :: IO SDL.Window
initializeWindow = do
  let windowCfg =
        SDL.defaultWindow
          { SDL.windowMode = SDL.FullscreenDesktop,
            SDL.windowPosition = SDL.Absolute $ SDL.P $ SDL.V2 0 0,
            SDL.windowInitialSize = SDL.V2 0 0,
            SDL.windowBorder = False
          }
  window <- SDL.createWindow "Chelleport" windowCfg
  SDL.showWindow window
  pure window
