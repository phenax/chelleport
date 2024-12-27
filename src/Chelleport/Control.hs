module Chelleport.Control where

import Chelleport.KeySequence (isAlphabetic, isKeycodeDigit, toKeyChar)
import Chelleport.Types
import Chelleport.Utils
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Maybe (fromMaybe)
import qualified Debug.Trace as Debug
import Foreign.C.Types
import qualified Graphics.X11 as X11
import qualified SDL

class (Monad m) => MonadControl m where
  clickMouseButton :: MouseButtonType -> m ()
  moveMousePointer :: CInt -> CInt -> m ()
  pressMouseButton :: m ()
  releaseMouseButton :: m ()
  getMousePointerPosition :: m (CInt, CInt)

foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeButtonEvent"
  xSimulateButtonEvent :: X11.Display -> X11.Button -> Bool -> X11.Time -> IO X11.Status

-- Wrap with delay to prevent async window close issues
withInteractionDelay :: (MonadIO m) => m () -> m ()
withInteractionDelay act = delay >> act >> delay
  where
    delay = liftIO (threadDelay 20_000)

instance (MonadIO m) => MonadControl (AppM m) where
  clickMouseButton btn = do
    (DrawContext {ctxX11Display = display}) <- ask
    withInteractionDelay . liftIO $ do
      xSimulateButtonEvent display x11Button True 0
      xSimulateButtonEvent display x11Button False 0
      X11.sync display False
    where
      x11Button = case btn of
        LeftClick -> X11.button1
        RightClick -> X11.button3

  moveMousePointer x y = do
    SDL.warpMouse SDL.WarpGlobal (SDL.P $ SDL.V2 x y)

  getMousePointerPosition = do
    DrawContext {ctxX11Display = display} <- ask
    liftIO $ do
      win <- X11.rootWindow display $ X11.defaultScreen display
      (success, _, _, x, y, _, _, _) <- X11.queryPointer display win
      unless success $ do
        Debug.traceM "ERROR: Cant query pointer"
      pure (x, y)

  pressMouseButton = do
    (DrawContext {ctxX11Display = display}) <- ask
    withInteractionDelay . liftIO $ do
      xSimulateButtonEvent display X11.button1 True 0
      X11.sync display False

  releaseMouseButton = do
    (DrawContext {ctxX11Display = display}) <- ask
    withInteractionDelay . liftIO $ do
      xSimulateButtonEvent display X11.button1 False 0
      X11.sync display False

eventToKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventToKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

keyModifier :: SDL.KeyboardEventData -> SDL.KeyModifier
keyModifier = SDL.keysymModifier . SDL.keyboardEventKeysym

checkKey :: [SDL.KeyboardEventData -> Bool] -> SDL.KeyboardEventData -> Bool
checkKey = (<&&>)

pressed :: SDL.KeyboardEventData -> Bool
pressed = (SDL.Pressed ==) . SDL.keyboardEventKeyMotion

released :: SDL.KeyboardEventData -> Bool
released = (SDL.Released ==) . SDL.keyboardEventKeyMotion

key :: SDL.Keycode -> SDL.KeyboardEventData -> Bool
key keycode = (keycode ==) . eventToKeycode

ctrl :: SDL.KeyboardEventData -> Bool
ctrl ev = SDL.keyModifierLeftCtrl (keyModifier ev) || SDL.keyModifierRightCtrl (keyModifier ev)

alt :: SDL.KeyboardEventData -> Bool
alt ev = SDL.keyModifierLeftAlt (keyModifier ev) || SDL.keyModifierRightAlt (keyModifier ev)

shift :: SDL.KeyboardEventData -> Bool
shift ev = SDL.keyModifierLeftShift (keyModifier ev) || SDL.keyModifierRightShift (keyModifier ev)

anyDigit :: SDL.KeyboardEventData -> Bool
anyDigit = isKeycodeDigit . eventToKeycode

anyAlphabetic :: SDL.KeyboardEventData -> Bool
anyAlphabetic = isAlphabetic . eventToKeycode

hjkl :: SDL.KeyboardEventData -> Bool
hjkl = (`elem` ("HJKL" :: String)) . fromMaybe ' ' . toKeyChar . eventToKeycode

hjklDirection :: Char -> Direction
hjklDirection = \case
  'H' -> DirLeft
  'L' -> DirRight
  'K' -> DirUp
  'J' -> DirDown
  _ -> DirUp

directionalIncrement :: (CInt, CInt) -> Direction -> (Int, Int)
directionalIncrement (incX, incY) = \case
  DirLeft -> (-cIntToInt incX, 0)
  DirRight -> (cIntToInt incX, 0)
  DirUp -> (0, -cIntToInt incY)
  DirDown -> (0, cIntToInt incY)
