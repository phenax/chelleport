module Chelleport.Draw where

import Chelleport.Types
import Chelleport.Utils (cIntToInt, intToCInt)
import Control.Monad.Reader (MonadIO, MonadReader (ask), asks)
import Data.Text (Text)
import qualified Data.Vector.Storable as Vector
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

class (Monad m) => MonadDraw m where
  drawLine :: (CInt, CInt) -> (CInt, CInt) -> m ()
  drawText :: (CInt, CInt) -> TextStyle -> Text -> m (CInt, CInt)
  drawCircle :: Int -> (CInt, CInt) -> m ()
  fillRect :: (CInt, CInt) -> (CInt, CInt) -> m ()
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

  fillRect (x, y) (w, h) = do
    renderer <- asks ctxRenderer
    let rect = SDL.Rectangle (SDL.P $ SDL.V2 x y) (SDL.V2 w h)
    SDL.fillRect renderer (Just rect)

  drawText (x, y) (TextStyle {textColor, textSize, textAlign}) text = do
    DrawContext {ctxRenderer = renderer, ctxFontSmall, ctxFontLarge} <- ask
    let font = case textSize of
          FontSM -> ctxFontSmall
          FontLG -> ctxFontLarge
    surface <- TTF.blended font textColor text
    texture <- SDL.createTextureFromSurface renderer surface
    SDL.freeSurface surface

    -- Get text dimensions
    textureInfo <- SDL.queryTexture texture
    let textWidth = SDL.textureWidth textureInfo
    let textHeight = SDL.textureHeight textureInfo

    let (left, top) = case textAlign of
          AlignLeft -> (x, y)
          AlignCenter -> (x - textWidth `div` 2, y)

    -- Render the texture
    SDL.copy renderer texture Nothing $
      Just (SDL.Rectangle (SDL.P $ SDL.V2 left top) (SDL.V2 textWidth textHeight))
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

fillRectVertices :: (MonadDraw m) => (CInt, CInt) -> (CInt, CInt) -> m ()
fillRectVertices (x1, y1) (x2, y2) = fillRect (x1, y1) (x2 - x1, y2 - y1)

cellSize :: (MonadDraw m) => State -> m (CInt, CInt)
cellSize (State {stateGridRows, stateGridCols}) = do
  (width, height) <- windowSize
  let wcell = width `div` intToCInt stateGridCols
  let hcell = height `div` intToCInt stateGridRows
  pure (wcell, hcell)

pointerPositionIncrement :: (MonadDraw m) => State -> m (CInt, CInt)
pointerPositionIncrement state = do
  (wcell, hcell) <- cellSize state
  if stateIsShiftPressed state
    then pure (wcell `div` 4, hcell `div` 4)
    else pure (wcell `div` 16, hcell `div` 16)

screenPositionFromCellPosition :: (MonadDraw m) => State -> (Int, Int) -> m (Int, Int)
screenPositionFromCellPosition state (row, col) = do
  (wcell, hcell) <- cellSize state
  let x = (wcell `div` 2) + wcell * intToCInt col
  let y = (hcell `div` 2) + hcell * intToCInt row
  (winx, winy) <- windowPosition
  pure (cIntToInt $ winx + x, cIntToInt $ winy + y)

wordPosition :: (MonadDraw m) => OCRMatch -> m (Int, Int)
wordPosition (OCRMatch {matchStartX, matchStartY}) = do
  (x, y) <- windowPosition
  pure (cIntToInt $ x + matchStartX, cIntToInt $ y + matchStartY)

drawHorizontalLine :: (MonadDraw m) => CInt -> m ()
drawHorizontalLine y = do
  (width, _) <- windowSize
  drawLine (0, y) (width, y)

drawVerticalLine :: (MonadDraw m) => CInt -> m ()
drawVerticalLine x = do
  (_, height) <- windowSize
  drawLine (x, 0) (x, height)
