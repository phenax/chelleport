module Chelleport.View (render) where

import Chelleport.Draw
import Chelleport.Types
import Chelleport.Utils (intToCInt, isEmpty, isNotEmpty)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Vector.Storable as Vector
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import Data.Maybe (isJust)

render :: State -> DrawContext -> IO ()
render state ctx = do
  renderGridLines state ctx

  (SDL.V2 width height) <- windowSize ctx
  let grid = stateGrid state
  let wcell = width `div` intToCInt (length $ head grid)
  let hcell = height `div` intToCInt (length grid)

  forM_ (zip [0 ..] grid) $ \(rowIndex, row) -> do
    forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
      let py = rowIndex * hcell + 10
      let px = colIndex * wcell + wcell `div` 2 - 20
      visible <- renderKeySequence ctx (stateKeySequence state) cell (px, py)
      when visible $ do
        renderTargetPoint ctx (colIndex * wcell + wcell `div` 2, rowIndex * hcell + hcell `div` 2)

renderKeySequence :: DrawContext -> KeySequence -> Cell -> (CInt, CInt) -> IO Bool
renderKeySequence ctx keySequence cell (px, py) = do
  let (matched, remaining)
        | keySequence `isPrefixOf` cell = splitAt (length keySequence) cell
        | otherwise = ("", cell)

  let textColor
        | isEmpty keySequence = Just colorWhite
        | isNotEmpty matched = Just colorHighlight
        | otherwise = Nothing

  widthRef <- newIORef 0
  unless (isEmpty matched) $ do
    (textWidth, _h) <- drawText ctx (SDL.V2 px py) colorLightGray $ Text.pack matched
    modifyIORef' widthRef (const textWidth)

  unless (isEmpty remaining) $ do
    case textColor of
      Just color -> do
        prevTextWidth <- readIORef widthRef
        let pos = px + prevTextWidth
        void $ drawText ctx (SDL.V2 pos py) color $ Text.pack remaining
      Nothing -> pure ()

  pure (isJust textColor)

renderGridLines :: State -> DrawContext -> IO ()
renderGridLines state ctx@(DrawContext {ctxRenderer = renderer}) = do
  (SDL.V2 width height) <- windowSize ctx
  let grid = stateGrid state
  let wcell = width `div` intToCInt (length $ head grid)
  let hcell = height `div` intToCInt (length grid)

  let rows = intToCInt $ length grid
  let columns = intToCInt $ length $ head grid
  forM_ [0 .. rows] $ \rowIndex -> do
    SDL.rendererDrawColor renderer $= colorGray
    drawHorizontalLine ctx (rowIndex * hcell + hcell `div` 2)
    SDL.rendererDrawColor renderer $= colorGridLines
    drawHorizontalLine ctx $ rowIndex * hcell
  forM_ [0 .. columns] $ \colIndex -> do
    SDL.rendererDrawColor renderer $= colorGray
    drawVerticalLine ctx (colIndex * wcell + wcell `div` 2)
    SDL.rendererDrawColor renderer $= colorGridLines
    drawVerticalLine ctx $ colIndex * wcell

  SDL.rendererDrawColor renderer $= colorAxisLines
  drawHorizontalLine ctx (rows * hcell `div` 2)
  drawVerticalLine ctx (columns * wcell `div` 2)

renderTargetPoint :: DrawContext -> (CInt, CInt) -> IO ()
renderTargetPoint (DrawContext {ctxRenderer = renderer}) (x, y) = do
  let renderedPoints = 16
  let radius = 2.0 :: Double
  let toTheta n = fromIntegral n * (2 * pi) / fromIntegral renderedPoints
      toPointOnCircle n =
        SDL.V2
          (x + round (radius * cos (toTheta n)))
          (y + round (radius * sin (toTheta n)))
  let points = Vector.generate renderedPoints (SDL.P . toPointOnCircle)
  SDL.rendererDrawColor renderer $= colorWhite
  SDL.drawPoints renderer points
  pure ()
