module Chelleport.View (render) where

import Chelleport.Draw
import Chelleport.Types
import Chelleport.Utils (intToCInt, isEmpty, isNotEmpty)
import Control.Monad (forM_, unless, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL

render :: State -> DrawContext -> IO ()
render state ctx = do
  renderGridLines state ctx

  (SDL.V2 width height) <- windowSize ctx
  let grid = stateCells state
  let wcell = width `div` intToCInt (length $ head grid)
  let hcell = height `div` intToCInt (length grid)

  forM_ (zip [0 ..] grid) $ \(rowIndex, row) -> do
    forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
      let py = rowIndex * hcell
      let px = colIndex * wcell
      renderKeySequence ctx (stateKeySequence state) cell (px, py)

renderKeySequence :: DrawContext -> KeySequence -> Cell -> (CInt, CInt) -> IO ()
renderKeySequence ctx keySequence cell (px, py) = do
  let (matched, remaining)
        | keySequence `isPrefixOf` cell = splitAt (length keySequence) cell
        | otherwise = ("", cell)

  let textColor
        | isEmpty keySequence = colorWhite
        | isNotEmpty matched = colorHighlight
        | otherwise = colorGray

  widthRef <- newIORef 0
  unless (isEmpty matched) $ do
    (textWidth, _h) <- drawText ctx (SDL.V2 px py) colorLightGray $ Text.pack matched
    modifyIORef' widthRef (const textWidth)

  unless (isEmpty remaining) $ do
    prevTextWidth <- readIORef widthRef
    let pos = px + prevTextWidth
    void $ drawText ctx (SDL.V2 pos py) textColor $ Text.pack remaining

renderGridLines :: State -> DrawContext -> IO ()
renderGridLines state ctx@(DrawContext {ctxRenderer = renderer}) = do
  (SDL.V2 width height) <- windowSize ctx
  let grid = stateCells state
  let wcell = width `div` intToCInt (length $ head grid)
  let hcell = height `div` intToCInt (length grid)

  SDL.rendererDrawColor renderer $= colorGridLines
  let rows = intToCInt $ length grid
  let columns = intToCInt $ length $ head grid
  forM_ [0 .. rows] $ \rowIndex -> do
    drawHorizontalLine ctx $ rowIndex * hcell
  forM_ [0 .. columns] $ \colIndex -> do
    drawVerticalLine ctx $ colIndex * wcell

  SDL.rendererDrawColor renderer $= colorAxisLines
  drawHorizontalLine ctx (rows * hcell `div` 2)
  drawVerticalLine ctx (columns * wcell `div` 2)
