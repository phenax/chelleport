module Chelleport.Context (initializeContext) where

import Chelleport.Config
import Chelleport.Types
import Data.ByteString (ByteString)
-- import Data.Time.Clock.System
-- import qualified Debug.Trace as Debug
import Data.FileEmbed (embedFileRelative)
import qualified Graphics.X11 as X11
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

initializeContext :: IO DrawContext
initializeContext = do
  -- Initialize SDL
  SDL.initialize [SDL.InitVideo, SDL.InitEvents]
  TTF.initialize

  window <- initializeWindow
  renderer <- initializeRenderer window
  (fontSm, fontLg) <- loadFonts

  display <- X11.openDisplay ""

  pure $
    DrawContext
      { ctxWindow = window,
        ctxRenderer = renderer,
        ctxFontLarge = fontLg,
        ctxFontSmall = fontSm,
        ctxX11Display = display
      }

rawFontData :: ByteString
rawFontData = $(embedFileRelative "./static/font.ttf")

loadFonts :: IO (TTF.Font, TTF.Font)
loadFonts = do
  fontSm <- TTF.decode rawFontData 14
  TTF.setStyle fontSm [TTF.Bold]
  fontLg <- TTF.decode rawFontData 24
  TTF.setStyle fontLg [TTF.Bold]
  pure (fontSm, fontLg)

initializeRenderer :: SDL.Window -> IO SDL.Renderer
initializeRenderer window = do
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer -- No hw acc: {SDL.rendererType = SDL.SoftwareRenderer}
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
  SDL.createWindow "Chelleport" windowCfg
