module Chelleport.Control where

import Chelleport.Types
import Chelleport.Utils (cIntToInt)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
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

isKeyPressed :: SDL.KeyboardEventData -> Bool
isKeyPressed = (SDL.Pressed ==) . SDL.keyboardEventKeyMotion

isKeyRelease :: SDL.KeyboardEventData -> Bool
isKeyRelease = (SDL.Released ==) . SDL.keyboardEventKeyMotion

eventToKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventToKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

isKeyPressWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyPressWith keyboardEvent keyCode =
  isKeyPressed keyboardEvent && eventToKeycode keyboardEvent == keyCode

isKeyReleaseWith :: SDL.KeyboardEventData -> SDL.Keycode -> Bool
isKeyReleaseWith keyboardEvent keyCode =
  isKeyRelease keyboardEvent && eventToKeycode keyboardEvent == keyCode

keyModifier :: SDL.KeyboardEventData -> SDL.KeyModifier
keyModifier = SDL.keysymModifier . SDL.keyboardEventKeysym

withShift :: SDL.KeyboardEventData -> Bool
withShift ev = SDL.keyModifierLeftShift (keyModifier ev) || SDL.keyModifierRightShift (keyModifier ev)

withCtrl :: SDL.KeyboardEventData -> Bool
withCtrl ev = SDL.keyModifierLeftCtrl (keyModifier ev) || SDL.keyModifierRightCtrl (keyModifier ev)

directionalIncrement :: (CInt, CInt) -> Char -> (Int, Int)
directionalIncrement (incX, incY) = \case
  'H' -> (-cIntToInt incX, 0)
  'L' -> (cIntToInt incX, 0)
  'K' -> (0, -cIntToInt incY)
  'J' -> (0, cIntToInt incY)
  _ -> undefined
