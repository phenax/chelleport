module Chelleport where

import Chelleport.AppShell (Action (AppAction, SysQuit), DrawContext (ctxWindow), setupAppShell)
import Chelleport.Draw (colorLightGray, colorWhite, renderText)
import Chelleport.KeySequence (eventToKeycode, generateKeyCells, isValidKey, nextChars, toKeyChar)
import Control.Monad (forM_, unless, void)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf)
import Data.Text (splitOn)
import qualified Data.Text as Text
import qualified SDL
import Unsafe.Coerce (unsafeCoerce)

data State = State
  { stateCells :: [[[Char]]],
    stateKeySequence :: [Char]
  }

data AppAction = FilterSequence SDL.Keycode | SetupGrid

open :: IO ()
open = setupAppShell initialState update eventToAction render

padded :: Int -> a -> [a] -> [a]
padded 0 _ ls = ls
padded n x ls
  | length ls > n = ls
  | otherwise = padded (n - 1) x (ls ++ [x])

initialState :: DrawContext -> IO State
initialState _ctx = do
  let cells = generateKeyCells (rows, columns) hintKeys
  pure $ State {stateCells = cells, stateKeySequence = []}
  where
    rows = 16
    columns = 16
    hintKeys = "ABCDEFGIMNOPRSTUVWXYZ"

render :: State -> DrawContext -> IO ()
render state ctx = do
  (SDL.V2 width height) <- SDL.get $ SDL.windowSize $ ctxWindow ctx
  let rows = stateCells state
  let wcell = width `div` unsafeCoerce (length $ head rows)
  let hcell = height `div` unsafeCoerce (length rows)

  forM_ (zip [0 ..] rows) $ \(rowIndex, row) -> do
    forM_ (zip [0 ..] row) $ \(colIndex, cell) -> do
      let w = colIndex * wcell
      let h = rowIndex * hcell
      let keySequence = stateKeySequence state
      let (matched, remaining) =
            if keySequence `isPrefixOf` cell
              then splitAt (length keySequence) cell
              else ("", cell)

      widthRef <- newIORef 0
      unless (null matched) $ do
        let pos = w
        (textWidth, _h) <- renderText ctx (SDL.V2 pos h) colorLightGray $ Text.pack matched
        modifyIORef' widthRef (const textWidth)
      unless (null remaining) $ do
        prevTextWidth <- readIORef widthRef
        let pos = w + prevTextWidth
        void $ renderText ctx (SDL.V2 pos h) colorWhite $ Text.pack remaining

update :: State -> DrawContext -> AppAction -> IO State
update state _ctx SetupGrid = pure state
update state _ctx (FilterSequence key) =
  case validChars >>= (\chars -> (,chars) <$> toKeyChar key) of
    Just (keyChar, validChars')
      | keyChar `elem` validChars' ->
          pure state {stateKeySequence = stateKeySequence state ++ [keyChar]}
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
      | isKeyPress ev && isValidKey (eventToKeycode ev) ->
          Just $ AppAction $ FilterSequence $ eventToKeycode ev
    _ -> Nothing

isKeyPress :: SDL.KeyboardEventData -> Bool
isKeyPress keyboardEvent =
  SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed

isKeyPressWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPressWith keyboardEvent keyCode =
  isKeyPress keyboardEvent && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == keyCode
