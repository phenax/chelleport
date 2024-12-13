module Chelleport.Draw where

import Chelleport.AppShell (DrawContext (ctxFont, ctxRenderer))
import Data.Text (Text)
import qualified SDL
import qualified SDL.Font as TTF

-- Render text to the screen
renderText :: DrawContext -> Text -> IO ()
renderText ctx text = do
  -- Render text in white
  surface <- TTF.blended (ctxFont ctx) (SDL.V4 255 255 255 255) text
  texture <- SDL.createTextureFromSurface (ctxRenderer ctx) surface
  SDL.freeSurface surface

  -- Get text dimensions
  textureInfo <- SDL.queryTexture texture
  let textWidth = SDL.textureWidth textureInfo
      textHeight = SDL.textureHeight textureInfo
      position =
        SDL.P (SDL.V2 ((640 - textWidth) `div` 2) ((480 - textHeight) `div` 2))

  -- Render the texture
  SDL.copy (ctxRenderer ctx) texture Nothing $ Just (SDL.Rectangle position (SDL.V2 textWidth textHeight))
  SDL.destroyTexture texture
