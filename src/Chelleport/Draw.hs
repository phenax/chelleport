module Chelleport.Draw where

import Chelleport.Types
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C (CInt)
import qualified SDL
import qualified SDL.Font as TTF

colorWhite :: SDL.V4 Word8
colorWhite = SDL.V4 255 255 255 255

colorLightGray :: SDL.V4 Word8
colorLightGray = SDL.V4 100 100 100 255

colorGray :: SDL.V4 Word8
colorGray = SDL.V4 55 52 65 200

colorAccent :: SDL.V4 Word8
colorAccent = SDL.V4 110 112 247 255

colorHighlight :: SDL.V4 Word8
colorHighlight = colorAccent

colorGridLines :: SDL.V4 Word8
colorGridLines = SDL.V4 127 29 29 150

colorAxisLines :: SDL.V4 Word8
colorAxisLines = SDL.V4 239 68 68 255

colorBackground :: SDL.V4 Word8
colorBackground = SDL.V4 15 12 25 0

drawText :: DrawContext -> SDL.V2 CInt -> SDL.V4 Word8 -> Text -> IO (CInt, CInt)
drawText ctx@(DrawContext {ctxRenderer = renderer}) position color text = do
  surface <- TTF.solid (ctxFont ctx) color text -- TTF.blended
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface

  -- Get text dimensions
  textureInfo <- SDL.queryTexture texture
  let textWidth = SDL.textureWidth textureInfo
  let textHeight = SDL.textureHeight textureInfo

  -- Render the texture
  SDL.copy renderer texture Nothing $
    Just (SDL.Rectangle (SDL.P position) (SDL.V2 textWidth textHeight))
  SDL.destroyTexture texture

  pure (textWidth, textHeight)

windowSize :: DrawContext -> IO (SDL.V2 CInt)
windowSize = SDL.get . SDL.windowSize . ctxWindow

drawHorizontalLine :: DrawContext -> CInt -> IO ()
drawHorizontalLine ctx@(DrawContext {ctxRenderer = renderer}) x = do
  (SDL.V2 width _height) <- windowSize ctx
  SDL.drawLine renderer (SDL.P $ SDL.V2 0 x) (SDL.P $ SDL.V2 width x)

drawVerticalLine :: DrawContext -> CInt -> IO ()
drawVerticalLine ctx@(DrawContext {ctxRenderer = renderer}) x = do
  (SDL.V2 _width height) <- windowSize ctx
  SDL.drawLine renderer (SDL.P $ SDL.V2 x 0) (SDL.P $ SDL.V2 x height)
