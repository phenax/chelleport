module Chelleport.View (render, renderKeySequence, renderGranularGrid, renderGridLines) where

import Chelleport.Config
import Chelleport.Draw
import Chelleport.Types
import Chelleport.Utils (intToCInt, isEmpty, isNotEmpty)
import Control.Monad (forM_, void, when)
import Data.List (isPrefixOf, (\\))
import qualified Data.Text as Text
import Foreign.C (CInt)

render :: (MonadDraw m) => State -> m ()
render state = case stateMode state of
  ModeHints -> renderHintsView state
  ModeSearch {searchFilteredWords, searchHighlightedIndex} ->
    renderSearchView state searchFilteredWords searchHighlightedIndex

getSearchText :: State -> String
getSearchText state = case stateMode state of
  ModeHints -> ""
  ModeSearch {searchInputText, searchFilteredWords, searchHighlightedIndex} ->
    "Searching (" ++ matchCount ++ "): " ++ searchInputText
    where
      matchCount
        | isEmpty searchFilteredWords = "0/0"
        | otherwise = show (searchHighlightedIndex + 1) ++ "/" ++ show (length searchFilteredWords)

renderSearchView :: (MonadDraw m) => State -> [OCRMatch] -> Int -> m ()
renderSearchView state matches highlightedIndex = do
  renderGridLines state

  forM_ (zip [0 ..] matches) $ \(index, OCRMatch {matchStartX, matchStartY, matchEndX, matchEndY}) -> do
    setDrawColor $ if highlightedIndex == index then colorAccent else colorLightGray
    fillRectVertices (matchStartX, matchStartY) (matchEndX, matchEndY)

  (w, h) <- windowSize
  drawText (w `div` 2, h `div` 2) colorAccent FontSM (Text.pack $ getSearchText state)
  pure ()

renderHintsView :: (MonadDraw m) => State -> m ()
renderHintsView state = do
  renderGridLines state

  (wcell, hcell) <- cellSize state

  forM_ (zip [0 ..] $ stateGrid state) $ \(rowIndex, row) -> forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
    let py = rowIndex * hcell + 10
    let px = colIndex * wcell + wcell `div` 2 - 20
    visible <- renderKeySequence (stateKeySequence state) cell (px, py)
    when visible $ do
      renderTargetMarker state (rowIndex, colIndex)
      when (stateIsMatched state) $ do
        renderGranularGrid state (rowIndex, colIndex)

renderKeySequence :: (MonadDraw m) => KeySequence -> Cell -> (CInt, CInt) -> m Bool
renderKeySequence keySequence cell (px, py) = do
  let (matched, remaining)
        | keySequence `isPrefixOf` cell = splitAt (length keySequence) cell
        | otherwise = ("", cell)

  let (textColor, isVisible)
        | isEmpty keySequence = (Just colorWhite, True)
        | isNotEmpty matched = (Just colorHighlight, True)
        | otherwise = (Nothing, False)

  previousTextWidth <-
    if isNotEmpty matched
      then fst <$> drawText (px, py) colorLightGray FontLG (Text.pack matched)
      else pure 0

  when (isNotEmpty remaining) $ case textColor of
    Just color -> do
      void $ drawText (px + previousTextWidth, py) color FontLG $ Text.pack remaining
    Nothing -> pure ()

  pure isVisible

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

renderTargetMarker :: (MonadDraw m) => State -> (CInt, CInt) -> m ()
renderTargetMarker state (row, col) = do
  (wcell, hcell) <- cellSize state
  let (x, y) = (col * wcell + wcell `div` 2, row * hcell + hcell `div` 2)
  setDrawColor colorWhite
  drawCircle 2 (x, y)

renderGranularGrid :: (MonadDraw m) => State -> (CInt, CInt) -> m ()
renderGranularGrid state (row, col) = do
  (wcell, hcell) <- cellSize state
  let (x, y) = (col * wcell + wcell `div` 2, row * hcell + hcell `div` 2)

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
