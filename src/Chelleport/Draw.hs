module Chelleport.Draw where

import Chelleport.Types
import Chelleport.Utils (intToCInt)
import Control.Monad.Reader (MonadIO, MonadReader (ask), asks)
import Data.Text (Text)
import qualified Data.Vector.Storable as Vector
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

class (Monad m) => MonadDraw m where
  drawLine :: (CInt, CInt) -> (CInt, CInt) -> m ()
  drawText :: (CInt, CInt) -> Color -> Text -> m (CInt, CInt)
  drawCircle :: Int -> (CInt, CInt) -> m ()
  setDrawColor :: Color -> m ()
  windowSize :: m (CInt, CInt)
  windowPosition :: m (CInt, CInt)

instance (MonadIO m) => MonadDraw (AppM m) where
  drawLine (x1, y1) (x2, y2) = do
    renderer <- asks ctxRenderer
    SDL.drawLine renderer (SDL.P $ SDL.V2 x1 y1) (SDL.P $ SDL.V2 x2 y2)

  setDrawColor color = do
    renderer <- asks ctxRenderer
    SDL.rendererDrawColor renderer $= color

  drawText (x, y) color text = do
    DrawContext {ctxRenderer = renderer, ctxFont = font} <- ask
    surface <- TTF.blended font color text
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface

    -- Get text dimensions
    textureInfo <- SDL.queryTexture texture
    let textWidth = SDL.textureWidth textureInfo
    let textHeight = SDL.textureHeight textureInfo

    -- Render the texture
    SDL.copy renderer texture Nothing $
      Just (SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 textWidth textHeight))
    SDL.destroyTexture texture

    pure (textWidth, textHeight)

  drawCircle radius (x, y) = do
    renderer <- asks ctxRenderer
    SDL.drawPoints renderer pointsOnACircle
    where
      pointsOnACircle = Vector.generate renderedPoints (SDL.P . toPointOnCircle)
      renderedPoints = radius * 7
      toTheta n = fromIntegral n * (2 * pi) / fromIntegral renderedPoints
      toPointOnCircle n =
        SDL.V2
          (x + round ((fromIntegral radius :: Float) * cos (toTheta n)))
          (y + round ((fromIntegral radius :: Float) * sin (toTheta n)))

  windowSize = do
    SDL.V2 x y <- asks ctxWindow >>= SDL.get . SDL.windowSize
    pure (x, y)

  windowPosition = do
    SDL.V2 x y <- asks ctxWindow >>= SDL.getWindowAbsolutePosition
    pure (x, y)

cellSize :: (MonadDraw m) => State -> m (CInt, CInt)
cellSize (State {stateGrid}) = do
  (width, height) <- windowSize
  let wcell = width `div` intToCInt (length $ head stateGrid)
  let hcell = height `div` intToCInt (length stateGrid)
  pure (wcell, hcell)

drawHorizontalLine :: (MonadDraw m) => CInt -> m ()
drawHorizontalLine y = do
  (width, _) <- windowSize
  drawLine (0, y) (width, y)

drawVerticalLine :: (MonadDraw m) => CInt -> m ()
drawVerticalLine x = do
  (_, height) <- windowSize
  drawLine (x, 0) (x, height)
