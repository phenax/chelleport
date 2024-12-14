module Chelleport where

import Chelleport.AppShell (Action (AppAction, SysQuit), hideWindow, setupAppShell, shutdownApp)
import Chelleport.Context (DrawContext (ctxRenderer, ctxWindow))
import Chelleport.Control (moveMouse, triggerMouseLeftClick)
import Chelleport.Draw (colorAxisLines, colorGridLines, colorLightGray, colorWhite, drawHorizontalLine, drawVerticalLine, renderText)
import Chelleport.KeySequence (Cell, KeyGrid, KeySequence, eventToKeycode, findMatchPosition, generateKeyCells, isValidKey, nextChars, toKeyChar)
import Control.Monad (forM_, unless, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import Foreign.C (CInt)
import SDL (($=))
import qualified SDL
import Unsafe.Coerce (unsafeCoerce)

data State = State
  { stateCells :: KeyGrid,
    stateKeySequence :: KeySequence
  }

data AppAction = FilterSequence SDL.Keycode | TriggerLeftClick | SetupGrid

open :: IO ()
open = setupAppShell initialState update eventToAction render

initialState :: DrawContext -> IO State
initialState _ctx = do
  let cells = generateKeyCells (rows, columns) hintKeys
  pure $ State {stateCells = cells, stateKeySequence = []}
  where
    rows = 16
    columns = 16
    hintKeys = "ABCDEFGHIJKLMNOPRSTUVWXYZ1234567890"

render :: State -> DrawContext -> IO ()
render state ctx = do
  (SDL.V2 width height) <- SDL.get $ SDL.windowSize $ ctxWindow ctx
  let grid = stateCells state
  let wcell = width `div` unsafeCoerce (length $ head grid)
  let hcell = height `div` unsafeCoerce (length grid)

  renderGridLines state ctx

  forM_ (zip [0 ..] grid) $ \(rowIndex, row) -> do
    let py = rowIndex * hcell
    forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
      let px = colIndex * wcell
      renderKeySequence ctx (stateKeySequence state) cell (px, py)

renderKeySequence :: DrawContext -> KeySequence -> Cell -> (CInt, CInt) -> IO ()
renderKeySequence ctx keySequence cell (px, py) = do
  let (matched, remaining) =
        if keySequence `isPrefixOf` cell
          then splitAt (length keySequence) cell
          else ("", cell)

  widthRef <- newIORef 0
  unless (null matched) $ do
    (textWidth, _h) <- renderText ctx (SDL.V2 px py) colorLightGray $ Text.pack matched
    modifyIORef' widthRef (const textWidth)

  unless (null remaining) $ do
    prevTextWidth <- readIORef widthRef
    let pos = px + prevTextWidth
    void $ renderText ctx (SDL.V2 pos py) colorWhite $ Text.pack remaining

renderGridLines :: State -> DrawContext -> IO ()
renderGridLines state ctx = do
  (SDL.V2 width height) <- SDL.get $ SDL.windowSize $ ctxWindow ctx
  let grid = stateCells state
  let wcell = width `div` unsafeCoerce (length $ head grid)
  let hcell = height `div` unsafeCoerce (length grid)

  SDL.rendererDrawColor (ctxRenderer ctx) $= colorGridLines
  let rows = unsafeCoerce $ length grid
  let columns = unsafeCoerce $ length $ head grid
  forM_ [0 .. rows] $ \rowIndex -> do
    drawHorizontalLine ctx $ rowIndex * hcell
  forM_ [0 .. columns] $ \colIndex -> do
    drawVerticalLine ctx $ colIndex * wcell

  SDL.rendererDrawColor (ctxRenderer ctx) $= colorAxisLines
  drawHorizontalLine ctx (rows * hcell `div` 2)
  drawVerticalLine ctx (columns * wcell `div` 2)

update :: State -> DrawContext -> AppAction -> IO State
update state _ctx SetupGrid = pure state
update state ctx TriggerLeftClick = do
  hideWindow ctx
  triggerMouseLeftClick ctx
  shutdownApp ctx
  pure state
update state ctx (FilterSequence key) =
  case validChars >>= (\chars -> (,chars) <$> toKeyChar key) of
    Just (keyChar, validChars')
      | keyChar `elem` validChars' -> do
          (SDL.V2 width height) <- SDL.get $ SDL.windowSize $ ctxWindow ctx
          let newKeySequence = stateKeySequence state ++ [keyChar]
          let rows = stateCells state
          let wcell = width `div` unsafeCoerce (length $ head rows)
          let hcell = height `div` unsafeCoerce (length rows)
          case findMatchPosition newKeySequence rows of
            Just (row, col) -> do
              moveMouse ctx (wcell * unsafeCoerce col) (hcell * unsafeCoerce row)
            Nothing -> pure ()
          pure state {stateKeySequence = newKeySequence}
    _ -> pure state
  where
    validChars = nextChars (stateKeySequence state) (stateCells state)

eventToAction :: State -> SDL.Event -> Maybe (Action AppAction)
eventToAction _state event =
  case SDL.eventPayload event of
    -- SDL.WindowShownEvent _ -> Just $ AppAction SetupGrid
    SDL.QuitEvent -> Just SysQuit
    SDL.KeyboardEvent ev
      | isKeyPressWith ev SDL.KeycodeQ -> Just SysQuit
      | isKeyPressWith ev SDL.KeycodeEscape -> Just SysQuit
      | isKeyPressWith ev SDL.KeycodeSpace -> Just $ AppAction TriggerLeftClick
      | isKeyPress ev && isValidKey (eventToKeycode ev) ->
          Just $ AppAction $ FilterSequence $ eventToKeycode ev
    _ -> Nothing

isKeyPress :: SDL.KeyboardEventData -> Bool
isKeyPress keyboardEvent =
  SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed

isKeyPressWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPressWith keyboardEvent keyCode =
  isKeyPress keyboardEvent && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode
