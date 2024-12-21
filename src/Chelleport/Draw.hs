module Chelleport.Draw where

import Chelleport.Types
import Chelleport.Utils (intToCInt)
import Control.Monad.Reader (MonadIO, MonadReader (ask))
import Data.Text (Text)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word8)
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import qualified SDL.Font as TTF

class (Monad m) => MonadDraw m where
  drawLine :: (CInt, CInt) -> (CInt, CInt) -> m ()
  drawText :: (CInt, CInt) -> SDL.V4 Word8 -> Text -> m (CInt, CInt)
  drawCircle :: Int -> (CInt, CInt) -> m ()
  setDrawColor :: SDL.V4 Word8 -> m ()
  windowSize :: m (CInt, CInt)
  windowPosition :: m (CInt, CInt)

instance (MonadIO m) => MonadDraw (AppM m) where
  drawLine (x1, y1) (x2, y2) = do
    DrawContext {ctxRenderer = renderer} <- ask
    SDL.drawLine renderer (SDL.P $ SDL.V2 x1 y1) (SDL.P $ SDL.V2 x2 y2)

  setDrawColor color = do
    DrawContext {ctxRenderer = renderer} <- ask
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
    DrawContext {ctxRenderer = renderer} <- ask
    let renderedPoints = radius * 7
    let toTheta n = fromIntegral n * (2 * pi) / fromIntegral renderedPoints
    let toPointOnCircle n =
          SDL.V2
            (x + round ((fromIntegral radius :: Float) * cos (toTheta n)))
            (y + round ((fromIntegral radius :: Float) * sin (toTheta n)))
    let points = Vector.generate renderedPoints (SDL.P . toPointOnCircle)
    SDL.drawPoints renderer points

  windowSize = do
    SDL.V2 x y <- ask >>= SDL.get . SDL.windowSize . ctxWindow
    pure (x, y)

  windowPosition = do
    SDL.V2 x y <- ask >>= SDL.getWindowAbsolutePosition . ctxWindow
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
colorGridLines = colorGray

colorFocusLines :: SDL.V4 Word8
colorFocusLines = colorLightGray

colorAxisLines :: SDL.V4 Word8
colorAxisLines = colorAccent

colorBackground :: SDL.V4 Word8
colorBackground = SDL.V4 15 12 25 0

colorFineGrainGrid :: SDL.V4 Word8
colorFineGrainGrid = SDL.V4 55 52 65 100
