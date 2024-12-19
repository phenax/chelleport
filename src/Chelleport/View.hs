module Chelleport.View (render) where

import Chelleport.Draw
import Chelleport.Types
import Chelleport.Utils (intToCInt, isEmpty, isNotEmpty)
import Control.Monad (forM_, void, when)
import Data.List (isPrefixOf, (\\))
import Data.Maybe (isJust)
import qualified Data.Text as Text
import Foreign.C (CInt)

render :: (MonadDraw m) => State -> m ()
render state = do
  renderGridLines state

  (wcell, hcell) <- cellSize state

  forM_ (zip [0 ..] $ stateGrid state) $ \(rowIndex, row) -> forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
    let py = rowIndex * hcell + 10
    let px = colIndex * wcell + wcell `div` 2 - 20
    visible <- renderKeySequence (stateKeySequence state) cell (px, py)
    when visible $ do
      renderTargetPoints state (rowIndex, colIndex)

renderKeySequence ::(MonadDraw m) => KeySequence -> Cell -> (CInt, CInt) -> m Bool
renderKeySequence keySequence cell (px, py) = do
  let (matched, remaining)
        | keySequence `isPrefixOf` cell = splitAt (length keySequence) cell
        | otherwise = ("", cell)

  let textColor
        | isEmpty keySequence = Just colorWhite
        | isNotEmpty matched = Just colorHighlight
        | otherwise = Nothing

  previousTextWidth <- if isNotEmpty matched
    then fst <$> drawText (px, py) colorLightGray (Text.pack matched)
    else pure 0

  when (isNotEmpty remaining) $ case textColor of
    Just color -> do
      let pos = px + previousTextWidth
      void $ drawText (pos, py) color $ Text.pack remaining
    Nothing -> pure ()

  pure (isJust textColor)

renderGridLines :: (MonadDraw m) => State -> m ()
renderGridLines state = do
  let grid = stateGrid state
  (wcell, hcell) <- cellSize state

  let rows = intToCInt $ length grid
  let columns = intToCInt $ length $ head grid
  forM_ [0 .. rows] $ \rowIndex -> do
    setDrawColor colorFocusLines
    drawHorizontalLine (rowIndex * hcell + hcell `div` 2)
    setDrawColor colorGridLines
    drawHorizontalLine $ rowIndex * hcell
  forM_ [0 .. columns] $ \colIndex -> do
    setDrawColor colorFocusLines
    drawVerticalLine (colIndex * wcell + wcell `div` 2)
    setDrawColor colorGridLines
    drawVerticalLine $ colIndex * wcell

  setDrawColor colorAxisLines
  drawHorizontalLine (rows * hcell `div` 2)
  drawVerticalLine (columns * wcell `div` 2)

renderTargetPoints :: (MonadDraw m) =>State -> (CInt, CInt) -> m ()
renderTargetPoints state (row, col) = do
  (wcell, hcell) <- cellSize state
  let (x, y) = (col * wcell + wcell `div` 2, row * hcell + hcell `div` 2)
  setDrawColor colorWhite
  drawCircle 2 (x, y)
  when (stateIsMatched state) $ do
    setDrawColor colorFineGrainGrid
    forM_ ([-8 .. 8] \\ [0]) $ \n -> do
      let px = x + n * wcell `div` 16
      drawLine (px, y - hcell `div` 2) (px, y + hcell `div` 2)
    forM_ ([-8 .. 8] \\ [0]) $ \n -> do
      let py = y + n * hcell `div` 16
      drawLine (x - wcell `div` 2, py) (x + wcell `div` 2, py)

    setDrawColor colorLightGray
    let lenx = wcell `div` 4
    let leny = hcell `div` 4
    drawLine (x - wcell `div` 4, y - leny) (x - wcell `div` 4, y + leny)
    drawLine (x + wcell `div` 4, y - leny) (x + wcell `div` 4, y + leny)
    drawLine (x - lenx, y - hcell `div` 4) (x + lenx, y - hcell `div` 4)
    drawLine (x - lenx, y + hcell `div` 4) (x + lenx, y + hcell `div` 4)
