module Chelleport.View (render) where

import Chelleport.Draw
import Chelleport.Types
import Chelleport.Utils (intToCInt, isEmpty, isNotEmpty)
import Control.Monad (forM_, unless, void, when)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, (\\))
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL

render :: State -> DrawContext -> IO ()
render state ctx = do
  renderGridLines state ctx

  (wcell, hcell) <- cellSize state ctx

  forM_ (zip [0 ..] $ stateGrid state) $ \(rowIndex, row) -> do
    forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
      let py = rowIndex * hcell + 10
      let px = colIndex * wcell + wcell `div` 2 - 20
      visible <- renderKeySequence ctx (stateKeySequence state) cell (px, py)
      when visible $ do
        renderTargetPoints state ctx (rowIndex, colIndex)

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
  let grid = stateGrid state
  (wcell, hcell) <- cellSize state ctx

  let rows = intToCInt $ length grid
  let columns = intToCInt $ length $ head grid
  forM_ [0 .. rows] $ \rowIndex -> do
    SDL.rendererDrawColor renderer $= colorFocusLines
    drawHorizontalLine ctx (rowIndex * hcell + hcell `div` 2)
    SDL.rendererDrawColor renderer $= colorGridLines
    drawHorizontalLine ctx $ rowIndex * hcell
  forM_ [0 .. columns] $ \colIndex -> do
    SDL.rendererDrawColor renderer $= colorFocusLines
    drawVerticalLine ctx (colIndex * wcell + wcell `div` 2)
    SDL.rendererDrawColor renderer $= colorGridLines
    drawVerticalLine ctx $ colIndex * wcell

  SDL.rendererDrawColor renderer $= colorAxisLines
  drawHorizontalLine ctx (rows * hcell `div` 2)
  drawVerticalLine ctx (columns * wcell `div` 2)

renderTargetPoints :: State -> DrawContext -> (CInt, CInt) -> IO ()
renderTargetPoints state ctx@(DrawContext {ctxRenderer = renderer}) (row, col) = do
  (wcell, hcell) <- cellSize state ctx
  let (x, y) = (col * wcell + wcell `div` 2, row * hcell + hcell `div` 2)
  SDL.rendererDrawColor renderer $= colorWhite
  drawCircle ctx 2 (x, y)
  when (stateIsMatched state) $ do
    SDL.rendererDrawColor renderer $= colorFineGrainGrid
    forM_ ([-8 .. 8] \\ [0]) $ \n -> do
      let px = x + n * wcell `div` 16
      SDL.drawLine renderer (SDL.P $ SDL.V2 px (y - hcell `div` 2)) (SDL.P $ SDL.V2 px (y + hcell `div` 2))
    forM_ ([-8 .. 8] \\ [0]) $ \n -> do
      let py = y + n * hcell `div` 16
      SDL.drawLine renderer (SDL.P $ SDL.V2 (x - wcell `div` 2) py) (SDL.P $ SDL.V2 (x + wcell `div` 2) py)

    SDL.rendererDrawColor renderer $= colorLightGray
    let lenx = wcell `div` 4
    let leny = hcell `div` 4
    SDL.drawLine renderer (SDL.P $ SDL.V2 (x - wcell `div` 4) (y - leny)) (SDL.P $ SDL.V2 (x - wcell `div` 4) (y + leny))
    SDL.drawLine renderer (SDL.P $ SDL.V2 (x + wcell `div` 4) (y - leny)) (SDL.P $ SDL.V2 (x + wcell `div` 4) (y + leny))
    SDL.drawLine renderer (SDL.P $ SDL.V2 (x - lenx) (y - hcell `div` 4)) (SDL.P $ SDL.V2 (x + lenx) (y - hcell `div` 4))
    SDL.drawLine renderer (SDL.P $ SDL.V2 (x - lenx) (y + hcell `div` 4)) (SDL.P $ SDL.V2 (x + lenx) (y + hcell `div` 4))
