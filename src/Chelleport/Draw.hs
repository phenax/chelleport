module Chelleport.Draw where

import Chelleport.AppShell (DrawContext (ctxFont, ctxRenderer))
import Data.Text (Text)
import Data.Word (Word8)
import Foreign.C (CInt)
import qualified SDL
import qualified SDL.Font as TTF

colorWhite :: SDL.V4 Word8
colorWhite = SDL.V4 255 255 255 255

colorLightGray :: SDL.V4 Word8
colorLightGray = SDL.V4 100 100 100 255

renderText :: DrawContext -> SDL.V2 CInt -> SDL.V4 Word8 -> Text -> IO (CInt, CInt)
renderText ctx position color text = do
  surface <- TTF.blended (ctxFont ctx) color text
  texture <- SDL.createTextureFromSurface (ctxRenderer ctx) surface
  SDL.freeSurface surface

  -- Get text dimensions
  textureInfo <- SDL.queryTexture texture
  let textWidth = SDL.textureWidth textureInfo
  let textHeight = SDL.textureHeight textureInfo

  -- Render the texture
  SDL.copy (ctxRenderer ctx) texture Nothing $
    Just (SDL.Rectangle (SDL.P position) (SDL.V2 textWidth textHeight))
  SDL.destroyTexture texture

  pure (textWidth, textHeight)
